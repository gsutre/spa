(* $Id: TestCore.mli 3275 2015-11-01 21:50:37Z sutre $ *)


(**
 * Minimalistic unit testing library.
 *
 * This module provides functions to create and run test cases, with a focus on
 * extreme simplicity and self-containedness.  It is inspired from the [OUnit]
 * testing framework for OCaml, but not nearly as good as it!
 *
 * A test case is given by a pair [(name, test)] where [name] is a [string] and
 * [test] is a function from [unit] to [unit].  A test case {e fails} when it
 * raises an exception signaling the failure, and it {e passes} otherwise.  Such
 * an exception can be manually raised by calling the function [failure].  But,
 * for clarity, it is recommended to use the [assert_]* functions instead.
 *
 * @see <http://ounit.forge.ocamlcore.org/>
 *   OUnit
 *)


(** {2 Main functions} *)

(**
 * [failure msg] signals a test failure.  The string [msg] is used to report the
 * failure.
 *)
val failure : string -> unit

(**
 * [run (name, tests)] executes the test suite called [name] and composed of the
 * list [tests] of test cases.  Returns [true] if all test cases passed, and
 * [false] otherwise.
 *)
val run : string * (string * (unit -> unit)) list -> bool


(** {2 Assertions} *)

(**
 * [assert_equal equal print msg expected actual] checks that [expected] and
 * [actual] are equal according to the equality predicate [equal].  If they are
 * not equal, [print] and [msg] are used to report the failure.
 *)
val assert_equal :
  equal:('a -> 'a -> bool) -> print:(Format.formatter -> 'a -> unit) ->
  ?msg:string -> 'a -> 'a -> unit

(**
 * [assert_bool] specializes [assert_equal] over booleans.  It uses structural
 * equality [(=)] and the standard pretty-printer [Format.pp_print_bool].
 *)
val assert_bool : ?msg:string -> bool -> bool -> unit

(**
 * [assert_int] specializes [assert_equal] over integers.  It uses structural
 * equality [(=)] and the standard pretty-printer [Format.pp_print_int].
 *)
val assert_int : ?msg:string -> int -> int -> unit

(**
 * [assert_string] specializes [assert_equal] over strings.  It uses structural
 * equality [(=)] and the standard pretty-printer [Format.pp_print_string].
 *)
val assert_string : ?msg:string -> string -> string -> unit

(**
 * [assert_print msg expected print actual] uses [assert_string] to check that
 * the string obtained by printing [actual] with the printer [print] is equal to
 * [expected].
 *)
val assert_print :
  ?msg:string -> string -> (Format.formatter -> 'a -> unit) -> 'a -> unit


(** {2 Configuration} *)

(**
 * Verbosity level of the library.  The default verbosity level is 1.  Higher
 * levels make the library output more messages.
 *)
val verbosity : int ref
