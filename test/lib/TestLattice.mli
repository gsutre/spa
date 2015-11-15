(* $Id: TestLattice.mli 3267 2015-10-31 16:41:05Z sutre $ *)


(**
 * Generic test cases for lattices.
 *
 * This module provides a functor that generates test cases for a given lattice
 * implementation.  The functor takes as input a module [L] implementating the
 * {! Lattice.S} interface, as well as:
 *
 * - a finite set E of elements of the lattice, and
 * - a binary relation R on E such that the reflexive-transitive closure R{^ *}
 *   coincides with the lattice's partial order on E.
 *
 * Typically, the binary relation R corresponds to the Hasse diagram of the
 * lattice's partial order restricted to E.  The generated test cases check the
 * following properties.  For simplicity, the [L.equal] and [L.leq] predicates
 * are written = and ⊑, respectively.
 *
 * - {b \[equal\]} The binary predicate = coincides with physical equality
 *   ([==]) on E.
 * - {b \[leq\]} The binary predicate ⊑ coincides with R{^ *} on E.
 * - {b \[leq.antisymmetry\]} It holds that (e ⊑ f ∧ f ⊑ e) ⇒ e = f, for every
 *   e, f ∈ E.
 * - {b \[bot\]} It holds that [L.bot] ⊑ e, for every e ∈ E.
 * - {b \[top\]} It holds that [L.top] ⊒ e, for every e ∈ E.
 * - {b \[glb.lowerbound\]} It holds that both ([L.glb e f]) ⊑ e and
 *   ([L.glb e f]) ⊑ f, for every e, f ∈ E.
 * - {b \[glb.greatest\]} It holds that ([L.glb e f]) ⊒ b for every e, f, b ∈ E
 *   such that b ⊑ e and b ⊑ f.
 * - {b \[lub.upperbound\]} It holds that both ([L.lub e f]) ⊒ e and
 *   ([L.lub e f]) ⊒ f, for every e, f ∈ E.
 * - {b \[lub.least\]} It holds that ([L.lub e f]) ⊑ b for every e, f, b ∈ E
 *   such that b ⊒ e and b ⊒ f.
 *)


(**
 * Input signature of the functor {!TestLattice.Make}.
 *)
module type I =
sig
  (**
   * Lattice implementation under test.
   *)
  module L : Lattice.S

  (**
   * The set E of elements on which to test the lattice implementation.  These
   * elements are {e expected} to be distinct w.r.t. [L.equal].  They {e must}
   * be physically distinct.
   *)
  val elements : L.t list

  (**
   * A binary relation whose reflexive-transitive closure is {e expected} to
   * coincide with [L.leq] on E.  The relation {e must} be contained in E × E.
   *)
  val relation : (L.t * L.t) list
end

module Make : functor (L : I) ->
sig
  (**
   * [tests prefix] returns test cases to check the lattice implementation [L].
   * The string [prefix] is prepended to each test case name.
   *)
  val tests : string -> (string * (unit -> unit)) list
end
