(* $Id: TestUtil.ml 3369 2015-11-13 13:06:25Z sutre $ *)


(*
 * Utility functions for unit tests.
 *)


(*
 * Numerical non-relational abstract domain testing.
 *)
module OfNumericalDomain (N : NumericalDomain.S) =
struct
  open TestCore
  open N

  (* Helper function to create test messages. *)
  let message name a b =
    Format.asprintf "@[<h>%s@ %a@ %a@]" name print a print b

  (* Variant of the above with an additional argument. *)
  let message' name a b c =
    Format.asprintf "@[<h>%s@ %a@ %a@ %a@]" name print a print b print c

  (* Specialized assert_equal for the type t. *)
  let assert_equal_t ~msg expected actual =
    assert_print ~msg expected print actual
end

(*
 * Numerical relational abstract domain testing.
 *)
module OfAbstractDomain (A : AbstractDomain.S) =
struct
  open TestCore
  open A

  (* Helper function to create test messages. *)
  let message name cmd a =
    Format.asprintf "@[<h>%s@ [%a]@ %a@]" name Command.print cmd print a

  (* Specialized assert_equal for the type t. *)
  let assert_equal_t ~msg expected actual =
    assert_print ~msg expected print actual

  (* Helper function that tests a sequence of commands. *)
  let sequence op name a commands expecteds =
    ignore
      (List.fold_left2
         (fun a cmd exp ->
          let b = op cmd a
          in
          assert_equal_t ~msg:(message name cmd a) exp b ; b)
         a
         commands
         expecteds)

  (* Shortcuts to write expressions. *)
  let cst c = Command.Expression.Cst c
  let var v = Command.Expression.Var v
  let ( + ) e e' = Command.Expression.Op (e, Command.Expression.Add, e')
  let ( - ) e e' = Command.Expression.Op (e, Command.Expression.Sub, e')
  let ( * ) e e' = Command.Expression.Op (e, Command.Expression.Mul, e')
  let ( / ) e e' = Command.Expression.Op (e, Command.Expression.Div, e')

  (* Shortcuts to write guards. *)
  let ( == ) e e' = Command.Guard (e, Command.Predicate.Eq, e')
  let ( < ) e e' = Command.Guard (e, Command.Predicate.Lst, e')
  let ( > ) e e' = Command.Guard (e, Command.Predicate.Gst, e')
  let ( <= ) e e' = Command.Guard (e, Command.Predicate.Leq, e')
  let ( >= ) e e' = Command.Guard (e, Command.Predicate.Geq, e')

  (* Shortcut to write assignments. *)
  let ( := ) v e =
    match v with
    | Command.Expression.Var v -> Command.Assign (v, e)
    | Command.Expression.Cst _
    | Command.Expression.Op _ -> invalid_arg "TestUtil.OfAbstractDomain.(:=)"
end
