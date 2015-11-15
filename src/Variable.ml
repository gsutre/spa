(* $Id: Variable.ml 3379 2015-11-15 16:05:22Z sutre $ *)


(*
 * Program Variables.
 *)


type t = string
let print = Format.pp_print_string
let compare = compare
let equal = (=)
let hash = Hashtbl.hash
