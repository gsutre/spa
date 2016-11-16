(* $Id: PointwiseLifting.ml 4064 2016-11-10 12:01:48Z sutre $ *)


(*
 * Pointwise lifting of a numerical non-relational abstract domain.
 *)


module Make (N : NumericalDomain.S) =
struct
  module VarMap = Map.Make (Variable)

  (*
   * Abstract elements are total functions from the set of variables to the set
   * of abstract numerical values.  These functions are partially ordered by the
   * componentwise extension of the partial order on abstract numerical values.
   *
   * Abstract elements are also called abstract contexts.  The concretization of
   * an abstract context a is the set of concrete environments ρ such that
   * ρ(v) ∈ γ(a(v)) for every variable v, where γ denotes the concretization
   * function of the numerical non-relational abstract domain N.
   *
   * Note: The underlying Galois connection is not a Galois insertion in general
   * since several abstract contexts may have an empty concretization.
   *
   * It is easily seen that all abstract contexts obtained by lattice operations
   * and abstract semantics operators are functions that differ from a constant
   * only for finitely many variables.  So this module encodes abstract contexts
   * by pairs (m, d) where:
   *
   * - m is a finite association table from variables to abstract numerical
   *   values, and
   * - d is a default abstract numerical value (for variables that are not
   *   mapped in m).
   *
   * The corresponding abstract context is the function a defined by a(v) = m(v)
   * if v is in the domain of m, and a(v) = d otherwise.
   *)
  type t = (N.t VarMap.t) * N.t

  let print fmt (m, d) =
    Format.fprintf fmt "{@[%a@]" N.print d ;
    VarMap.iter
      (fun v x ->
       Format.fprintf
         fmt ",@ @[%a@ ↦@ %a@]"
         Variable.print v N.print x)
      m ;
    Format.fprintf fmt "}"

  (* Gets the abstract numerical value of the variable v. *)
  let get (m, d) v =
    try VarMap.find v m with Not_found -> d

  (* Sets the value of the variable v to the abstract numerical value x. *)
  let set (m, d) v x =
    if N.equal x d then
      (* Minimize the table (but this is not mandatory). *)
      (VarMap.remove v m, d)
    else
      (VarMap.add v x m, d)

  (* Least and Greatest elements. *)
  let bot = (VarMap.empty, N.bot)
  let top = (VarMap.empty, N.top)

  (* Partial order. *)
  let leq (m, d) (n, e) =
    (* Simple but non-optimal: common variables are tested twice. *)
    N.leq d e &&
      VarMap.for_all (fun v x -> N.leq x (get (n, e) v)) m &&
        VarMap.for_all (fun v y -> N.leq (get (m, d) v) y) n

  (* Equality. *)
  let equal a b = (leq a b) && (leq b a)

  (* Generic pointwise lifting of an operator op : N.t -> N.t -> N.t. *)
  let lift op (m, d) (n, e) =
    let f = op d e
    in
    (* Helper function to minimize the table (but this is not mandatory). *)
    let filter x = if N.equal x f then None else Some x
    in
    let merge_aux _ xo yo =
      match (xo, yo) with
      | (None, None) -> None
      | (Some x, None) -> filter (op x e)
      | (None, Some y) -> filter (op d y)
      | (Some x, Some y) -> filter (op x y)
    in
    (VarMap.merge merge_aux m n, f)

  (* Greatest lower bound. *)
  let glb = lift N.glb

  (* Least upper bound. *)
  let lub = lift N.lub

  (* Emptiness test. *)
  let empty (m, d) =
    N.empty d ||
      VarMap.exists (fun _ x -> N.empty x) m

  (* Evaluation of an expression. *)
  let rec eval a =
    function
    | Command.Expression.Cst c -> N.abs c
    | Command.Expression.Var v -> get a v
    | Command.Expression.Op (e, o, e') ->
       let fun_op =
         match o with
         | Command.Expression.Add -> N.Op.add
         | Command.Expression.Sub -> N.Op.sub
         | Command.Expression.Mul -> N.Op.mul
         | Command.Expression.Div -> N.Op.div
       in
       fun_op (eval a e) (eval a e')

  (*
   * Abstract post-image of an assignment.
   *
   * Performing v := e forwards, starting from a non-empty abstract context a,
   * amounts to replacing the value of v by the evaluation of e in the abstract
   * context a.
   *)
  let post_assign v e a =
    if empty a then bot
    else
      let x = eval a e
      in
      (* The evaluation could return an empty abstract numerical value (/0). *)
      if N.empty x then
        bot
      else
        set a v x

  (*
   * Linearization of a given expression by partial evaluation of the variables.
   * Consider an abstract context a, an expression e and a variable v.  Let us
   * collect all expressions obtained from e by replacing each variable u ≠ v by
   * a constant in γ(a(u)).  The resulting expressions are univariate, so we may
   * see each one of them as a partial function from ℤ to ℤ.  The function is
   * partial to account for the forbidden division by zero.  Collecting these
   * functions leads to a function f : ℤ → ℘(ℤ).
   *
   * linearize e a v returns a pair (r, s), standing for r*v + s, that conserva-
   * tively approximates f.  This means that for every concrete environment ρ in
   * γ(a), the concrete evaluation of e under ρ (if any) is contained in the set
   * (γ(r) * {ρ(v)}) + γ(s).  Here, arithmetic operations are extended to sets
   * of integers in the usual way (e.g., X+Y is {x+y | x ∈ X ∧ y ∈ Y}).
   *)
  let rec linearize e a v =
    match e with
    | Command.Expression.Cst c -> (N.abs 0, N.abs c)
    | Command.Expression.Var u ->
       if Variable.equal u v then
         (N.abs 1, N.abs 0)
       else
         (N.abs 0, get a u)
    | Command.Expression.Op (e, o, e') ->
       let (r, s) = linearize e a v
       and (r', s') = linearize e' a v
       in
       match o with
       | Command.Expression.Add ->
          (N.Op.add r r', N.Op.add s s')
       | Command.Expression.Sub ->
          (N.Op.sub r r', N.Op.sub s s')
       | Command.Expression.Mul ->
          (* (r*v + s) * (r'*v + s') ⊑ (r*r'*a(v) + r*s' + s*r')*v + s*s'. *)
          let r'' =
            N.Op.add
              (N.Op.mul
                 (N.Op.mul r r')
                 (get a v))
              (N.Op.add
                 (N.Op.mul r s')
                 (N.Op.mul s r'))
          and s'' = N.Op.mul s s'
          in
          (r'', s'')
       | Command.Expression.Div ->
          (*
           * Linearizations of the form r*v + s are not enough to handle integer
           * division, since r denotes a set of integers.
           *
           * Take the following example, with intervals.  We are processing the
           * guard (v / 4) == 2.
           *
           * - current value of v: [8, 100]
           * - expression:         v/4 - 2
           *
           * The linearization as r*v + s would be [0, 1]*v + [-2, -2].  This
           * expression cannot evaluate to 0 when v ranges over [8, 100].  The
           * problem is that the interval [0, 1] denotes a set of integer, but
           * it should be interpreted, here, as a set of rational numbers.
           *
           * So we fully evaluate both operands by replacing v with its value
           * according to a.
           *)
          let s'' =
            N.Op.div
              (N.Op.add (N.Op.mul r (get a v)) s)
              (N.Op.add (N.Op.mul r' (get a v)) s')
          in
          (N.abs 0, s'')

  (*
   * Helper function for post_guard and pre_assign.
   *
   * feasible e solver y a conservatively tests whether there exists a concrete
   * environment ρ ∈ γ(a) and an integer z ∈ γ(y) such that ρ ⊧ e ⋈ z, where ⋈
   * is = or ≤ depending on solver.  The latter is either N.Op.equality or
   * N.Op.inequality.
   *
   * Note: The y argument slightly complicates this function and is useless for
   * post_guard.  But it is useful for pre_assign.
   *)
  let feasible e solver y a =
    if empty a then false
    else
      let x = eval a e
      in
      if N.empty x then
        false
      else
        (* Conservatively test whether x ⋈ y. *)
        not (N.empty (solver (N.abs 0) (N.Op.sub x y) N.top))

  (*
   * Helper function for post_guard and pre_assign.
   *
   * refine e solver y a v refines the value of the variable v in the abstract
   * context a to account for the condition e ⋈ y, where ⋈ is = or ≤ depending
   * on solver.
   *
   * Ideally, the refined value of v is α(S), where S is the projection on v of
   * the set of concrete environments in γ(a) that satisfy e ⋈ y.  Formally,
   * S = {ρ(v) | ρ ∈ γ(a) ∧ ∃ z ∈ γ(y) : ρ ⊧ e ⋈ z}.  The ideal refinement α(S)
   * is not always computable, so we settle for a conservative approximation of
   * α(S), obtained as follows:
   *
   * - Compute the linearization (r, s) of e with respect to a and v,
   * - Solve the equality or inequality r*v + (s-y) ⋈ 0 subject to v ⊑ a(v),
   *   using solver.
   *
   * The resulting abstract numerical value is guaranteed to be greater than or
   * equal to α(S).
   *
   * Note: The y argument slightly complicates this function and is useless for
   * post_guard.  But it is useful for pre_assign.
   *)
  let refine e solver y a v =
    if leq a bot then bot
    else
      let (r, s) = linearize e a v
      in
      let x = solver r (N.Op.sub s y) (get a v)
      in
      assert (N.leq x (get a v)) ;
      if N.empty x then
        bot
      else
        set a v x

  (* Convenient shortcuts. *)
  let expr_add e e' =
    Command.Expression.Op (e, Command.Expression.Add, e')
  and expr_sub e e' =
    Command.Expression.Op (e, Command.Expression.Sub, e')
  and expr_cst c =
    Command.Expression.Cst c

  (* Abstract post-image of a guard. *)
  let post_guard (e, o, e') a =
    if leq a bot then bot
    else
      let (solver, expr) =
        match o with
        | Command.Predicate.Eq ->
           (N.Op.equality, expr_sub e e')
        | Command.Predicate.Lst ->
           (N.Op.inequality, expr_add (expr_sub e e') (expr_cst 1))
        | Command.Predicate.Gst ->
           (N.Op.inequality, expr_add (expr_sub e' e) (expr_cst 1))
        | Command.Predicate.Leq ->
           (N.Op.inequality, expr_sub e e')
        | Command.Predicate.Geq ->
           (N.Op.inequality, expr_sub e' e)
      and zero = N.abs 0
      in
      (* The guard is equivalent to expr ⋈ 0, where ⋈ is given by solver. *)
      if feasible expr solver zero a then
        (*
         * It is legitimate to fold (instead of refining each variable with a).
         * Indeed, in the concrete semantics, executing a guard g is equivalent
         * to executing g many times.
         *)
        List.fold_left
          (refine expr solver zero)
          a
          (Command.Predicate.variables (e, o, e'))
      else
        bot

  (* Abstract post-image operator required by AbstractDomain.S. *)
  let post =
    function
    | Command.Assign (v, e) -> post_assign v e
    | Command.Guard p -> post_guard p
    | Command.Skip -> fun a -> a

  (*
   * Abstract pre-image of an assignment.
   *
   * For an assignment v := e, the concrete pre-image of a formula φ (denoting a
   * set of concrete environments) is denoted by the formula:
   *
   *                       ∃ v' : (v' = e  ∧  φ[v ← v'])
   *
   * So performing v := e backwards, starting from a non-empty abstract context
   * a, amounts to executing the “guard” e ∈ γ(a(v)) from the abstract context b
   * that is obtained from a by replacing the value of v by ⊤.
   *)
  let pre_assign v e a =
    if empty a then bot
    else
      let y = get a v
      and b = set a v N.top
      in
      (* The following is inspired from the implementation of post_guard. *)
      if feasible e N.Op.equality y b then
        List.fold_left
          (refine e N.Op.equality y)
          b
          (Command.Expression.variables e)
      else
        bot

  (* Abstract pre-image of a guard. *)
  let pre_guard = post_guard

  (* Abstract pre-image operator required by AbstractDomain.S. *)
  let pre =
    function
    | Command.Assign (v, e) -> pre_assign v e
    | Command.Guard p -> pre_guard p
    | Command.Skip -> fun a -> a

  (* Widening. *)
  let widen = lift N.widen

  (* Narrowing. *)
  let narrow = lift N.narrow
end
