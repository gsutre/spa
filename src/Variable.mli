(* $Id: Variable.mli 3251 2015-10-29 15:56:15Z sutre $ *)


(**
 * Program Variables.
 *
 * Variables are totally ordered and hashable.
 *
 * @see <http://caml.inria.fr/pub/docs/manual-ocaml/libref/Set.OrderedType.html>
 *   Set.OrderedType
 * @see <http://caml.inria.fr/pub/docs/manual-ocaml/libref/Hashtbl.HashedType.html>
 *   Hashtbl.HashedType
 *)


(**
 * The printable type of variables.
 *)
include Print.S with type t = string

(**
 * Total ordering on variables.
 *)
val compare : t -> t -> int

(**
 * Equality on variables.
 *)
val equal : t -> t -> bool

(**
 * Hashing on variables.
 *)
val hash : t -> int
