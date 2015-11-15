(* $Id: Command.mli 3321 2015-11-08 21:53:24Z sutre $ *)


(**
 * Program Commands.
 *
 * This module defines the syntax of program “instructions” (see {! Automaton}).
 * These are called {e commands}, and are either assignments {i x} := {i exp},
 * or guards {i exp{_ 1}} ⋈ {i exp{_ 2}}, or the no-op instruction {i skip}.
 *)


(**
 * Arithmetical expressions are build from integer constants, variables, and
 * binary arithmetic operators.
 *)
module Expression :
sig
  type op = Add | Sub | Mul | Div

  type t =
    | Cst of int
    | Var of Variable.t
    | Op of t * op * t

  include Print.S with type t := t

  (**
   * [variables e] returns the variables occurring in the expression [e].
   *)
  val variables : t -> Variable.t list
end

(**
 * Atomic predicates are comparisons between two expressions.
 *)
module Predicate :
sig
  type op = Eq | Lst | Gst | Leq | Geq

  type t = Expression.t * op * Expression.t

  include Print.S with type t := t

  (**
   * [variables p] returns the variables occurring in the predicate [p].
   *)
  val variables : t -> Variable.t list
end

(**
 * Program commands are either assignments, or guards, or the no-op instruction.
 *)
type t =
  | Assign of Variable.t * Expression.t
  | Guard of Predicate.t
  | Skip

include Print.S with type t := t
