(* $Id: Command.ml 3379 2015-11-15 16:05:22Z sutre $ *)


(*
 * Program Commands.
 *)


module VarSet = Set.Make (Variable)

module Expression =
struct
  type op = Add | Sub | Mul | Div

  let string_of_op =
    function
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"

  type t =
    | Cst of int
    | Var of Variable.t
    | Op of t * op * t

  let rec print fmt =
    function
    | Cst c -> Format.pp_print_int fmt c
    | Var v -> Variable.print fmt v
    | Op (e, o, e') ->
       Format.fprintf
         fmt "(%a@ %s@ %a)"
         print e (string_of_op o) print e'

  let rec varset =
    function
    | Cst _ -> VarSet.empty
    | Var v -> VarSet.singleton v
    | Op (e, _, e') -> VarSet.union (varset e) (varset e')

  let variables e =
    VarSet.elements (varset e)
end

module Predicate =
struct
  type op = Eq | Lst | Gst | Leq | Geq

  let string_of_op =
    function
    | Eq -> "=="
    | Lst -> "<"
    | Gst -> ">"
    | Leq -> "<="
    | Geq -> ">="

  type t = Expression.t * op * Expression.t

  let print fmt (e, o, e') =
    Format.fprintf
      fmt "%a@ %s@ %a"
      Expression.print e (string_of_op o) Expression.print e'

  let variables (e, _, e') =
    VarSet.elements
      (VarSet.union (Expression.varset e) (Expression.varset e'))
end

type t =
  | Assign of Variable.t * Expression.t
  | Guard of Predicate.t
  | Skip

let print fmt =
  function
  | Assign (v, e) ->
     Format.fprintf
       fmt "%a@ :=@ %a"
       Variable.print v Expression.print e
  | Guard p ->
     Predicate.print fmt p
  | Skip ->
     Format.fprintf fmt "skip"
