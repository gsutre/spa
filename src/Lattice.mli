(* $Id: Lattice.mli 3270 2015-10-31 17:11:38Z sutre $ *)


(**
 * Lattices.
 *
 * A {e lattice} is a partially-ordered set such that every pair of elements has
 * a greatest lower bound and a least upper bound.  In addition, we require the
 * existence of a least element ⊥ and of a greatest element ⊤.
 *
 * {L {b Extended by:} {! AbstractDomain}, {! NumericalDomain}.}
 *
 * @see <http://en.wikipedia.org/wiki/Lattice_(order)>
 *   Wikipedia's article on lattices
 *)


(**
 * Common interface to all lattice implementations.
 *)
module type S =
sig
  (**
   * The printable type of elements of the lattice.
   *)
  include Print.S

  (**
   * Least element ⊥.
   *)
  val bot : t

  (**
   * Greatest element ⊤.
   *)
  val top : t

  (**
   * [equal a b] tests whether [a] and [b] are equal.
   *)
  val equal : t -> t -> bool

  (**
   * [leq a b] tests whether [a] is lesser than or equal to [b].
   *)
  val leq : t -> t -> bool

  (**
   * [glb a b] returns the greatest lower bound of [a] and [b].
   *)
  val glb : t -> t -> t

  (**
   * [lub a b] returns the least upper bound of [a] and [b].
   *)
  val lub : t -> t -> t
end
