(* $Id: DomInterval.ml 3379 2015-11-15 16:05:22Z sutre $ *)


(*
 * Numerical domain for range analysis.
 *)


open Eint

type t =
  | Bot                (* empty interval *)
  | Int of eint * eint (* non-empty interval *)

(* Checks that a value of type t is a legal abstract interval. *)
let legal =
  function
  | Bot -> true
  | Int (l, u) -> (l < Pos_infty) && (Neg_infty < u) && (l <= u)

let print fmt =
  function
  | Bot -> Format.pp_print_string fmt "⊥"
  | Int (l, u) -> Format.fprintf fmt "[%a,@ %a]" pp_print_eint l pp_print_eint u

let bot = Bot
let top = Int (Neg_infty, Pos_infty)

let equal a b =
  assert ((legal a) && (legal b)) ;
  match (a, b) with
  | (Bot, Bot) -> true
  | (Int (l, u), Int (m, v)) -> (l = m) && (u = v)
  | (Bot, Int _)
  | (Int _, Bot) -> false

let leq a b =
  assert ((legal a) && (legal b)) ;
  match (a, b) with
  | (Bot, _) -> true
  | (_, Bot) -> false
  | (Int (l, u), Int (m, v)) -> (m <= l) && (u <= v)

let glb a b =
  assert ((legal a) && (legal b)) ;
  match (a, b) with
  | (Bot, _)
  | (_, Bot) -> Bot
  | (Int (l, u), Int (m, v)) ->
     let l' = max l m
     and u' = min u v
     in
     if l' <= u' then Int (l', u') else Bot

let lub a b =
  assert ((legal a) && (legal b)) ;
  match (a, b) with
  | (Bot, c)
  | (c, Bot) -> c
  | (Int (l, u), Int (m, v)) ->
     let l' = min l m
     and u' = max u v
     in
     Int (l', u')

let abs x = Int (Integer x, Integer x)

let empty a =
  assert (legal a) ;
  equal a bot

module Op =
struct
  let add a b =
    assert ((legal a) && (legal b)) ;
    match (a, b) with
    | (Bot, _)
    | (_, Bot) -> Bot
    | (Int (l, u), Int (m, v)) -> Int (l+m, u+v)

  let sub a b =
    assert ((legal a) && (legal b)) ;
    match (a, b) with
    | (Bot, _)
    | (_, Bot) -> Bot
    | (Int (l, u), Int (m, v)) -> Int (l-v, u-m)

  let mul a b =
    assert ((legal a) && (legal b)) ;
    match (a, b) with
    | (Bot, _)
    | (_, Bot) -> Bot
    | (Int (l, u), Int (m, v)) ->
       if (u < Integer 0) then
         (* l ≤ u < 0 *)
         let l' = if v > Integer 0 then l*v else u*v
         and u' = if m < Integer 0 then l*m else u*m
         in
         Int (l', u')
       else if (l > Integer 0) then
         (* 0 < l ≤ u *)
         let l' = if m < Integer 0 then u*m else l*m
         and u' = if v > Integer 0 then u*v else l*v
         in
         Int (l', u')
       else
         (* l ≤ 0 ≤ u *)
         Int (min (l*v) (u*m), max (l*m) (u*v))

  (* Returns the unary negation of an abstract interval. *)
  let minus a =
    assert (legal a) ;
    match a with
    | Bot -> Bot
    | Int (l, u) -> Int (~- u, ~- l)

  (* Division by an abstract interval b contained in the positive integers. *)
  let div_pos a b =
    assert ((legal a) && (legal b)) ;
    match (a, b) with
    | (Bot, _)
    | (_, Bot) -> Bot
    | (Int (l, u), Int (m, v)) ->
       assert (m > Integer 0) ;
       (* 0 < m ≤ v *)
       let l' = if l > Integer 0 then l/v else l/m
       and u' = if u < Integer 0 then u/v else u/m
       in
       Int (l', u')

  let div a b =
    assert ((legal a) && (legal b)) ;
    let b_neg = glb b (Int (Neg_infty, Integer (-1)))
    and b_pos = glb b (Int (Integer 1, Pos_infty))
    in
    lub (div_pos (minus a) (minus b_neg)) (div_pos a b_pos)

  (*
   * Perfect division by an abstract interval b contained in the positive
   * integers.
   *
   * The perfect division of an interval a by an interval b is the abstraction
   * of D = {x/y | x ∈ γ([a]) ∧ y ∈ γ([b]) ∧ y≠0 ∧ x ∈ yℤ\}.  The set D is also
   * the set of integers d such that d * (γ([b]) \ {0}) intersects (γ([a])).
   * This implementation returns an interval that conservatively approximates
   * the abstraction of D.
   *)
  let perfect_div_pos a b =
    assert ((legal a) && (legal b)) ;
    match (a, b) with
    | (Bot, _)
    | (_, Bot) -> Bot
    | (Int (l, u), Int (m, v)) ->
       assert (m > Integer 0) ;
       (* Solve [m, v] * d ≥ l. *)
       let d_min =
         if l > Integer 0 then
           (/) ~rounding:Pos_infty l v
         else
           (/) ~rounding:Pos_infty l m
       (* Solve [m, v] * d ≤ u. *)
       and d_max =
         if u < Integer 0 then
           (/) ~rounding:Neg_infty u v
         else
           (/) ~rounding:Neg_infty u m
       in
       if d_min <= d_max then
         Int (d_min, d_max)
       else
         Bot

  let equality a b c =
    assert ((legal a) && (legal b) && (legal c)) ;
    let a_neg = glb a (Int (Neg_infty, Integer (-1)))
    and a_pos = glb a (Int (Integer 1, Pos_infty))
    in
    if leq (abs 0) a && leq (abs 0) b then
      c
    else
      let sol_a_neg = glb (perfect_div_pos b (minus a_neg)) c
      and sol_a_pos = glb (perfect_div_pos (minus b) a_pos) c
      in
      lub sol_a_neg sol_a_pos

  let inequality a b c =
    let b' = add b (Int (Integer 0, Pos_infty))
    in
    equality a b' c
end

let widen a b =
  assert ((legal a) && (legal b)) ;
  match (a, b) with
  | (Bot, c)
  | (c, Bot) -> c
  | (Int (l, u), Int (m, v)) ->
     let l' = if m < l then Neg_infty else l
     and u' = if v > u then Pos_infty else u
     in
     Int (l', u')

let narrow a b =
  assert ((legal a) && (legal b)) ;
  assert (leq b a) ;
  match (a, b) with
  | (Bot, _)
  | (_, Bot) -> Bot
  | (Int (l, u), Int (m, v)) ->
     let l' = if l <= Neg_infty then m else l
     and u' = if u >= Pos_infty then v else u
     in
     Int (l', u')
