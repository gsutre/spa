(* $Id: NumericalDomain.mli 3367 2015-11-13 09:31:53Z sutre $ *)


(**
 * Numerical Non-Relational Abstract Domains.
 *
 * A numerical non-relational abstract domain is an abstract lattice that is in
 * Galois connection with the concrete lattice (℘(ℤ), ⊆).  The domain comes with
 * abstract numerical operations, and is equipped with convergence acceleration
 * operators (widening and narrowing).
 *
 * {L {b Implemented by:} {! DomConstant}, {! DomSign}, {! DomInterval}.}
 *)


(**
 * Common interface to all numerical non-relational abstract domains.
 *)
module type S =
sig
  (**
   * Underlying lattice (A, ⊑).
   *)
  include Lattice.S

  (**
   * Abstraction function α (for singletons).
   *)
  val abs : int -> t

  (**
   * [empty a] tests whether the concretization of [a] is the empty set.
   *)
  val empty : t -> bool

  (**
   * Abstract numerical operations.
   *)
  module Op :
  sig
    (**
     * [add a b] returns the abstraction of
     * \{x+y | x ∈ γ([a]) ∧ y ∈ γ([b])\}.
     *)
    val add : t -> t -> t

    (**
     * [sub a b] returns the abstraction of
     * \{x-y | x ∈ γ([a]) ∧ y ∈ γ([b])\}.
     *)
    val sub : t -> t -> t

    (**
     * [mul a b] returns the abstraction of
     * \{x*y | x ∈ γ([a]) ∧ y ∈ γ([b])\}.
     *)
    val mul : t -> t -> t

    (**
     * [div a b] returns the abstraction of
     * \{x/y | x ∈ γ([a]) ∧ y ∈ γ([b]) ∧ y≠0\}.
     *)
    val div : t -> t -> t

    (**
     * [equality a b c] solves the “linear equality” [a]*x + [b] = 0 subject to
     * the condition that x ⊑ [c].  It returns an abstract element that is
     * greater than or equal to the abstraction of the set of solutions of the
     * equality, namely the set
     * S = \{x ∈ γ([c]) | ∃ r ∈ γ([a]), ∃ s ∈ γ([b]) : r*x + s = 0\}.
     * Smaller abstract elements are better.
     * Ideally, the returned abstract element is α(S).
     *)
    val equality : t -> t -> t -> t

    (**
     * [inequality a b c] solves the “linear inequality” [a]*x + [b] ≤ 0 subject
     * to the condition that x ⊑ [c].  It returns an abstract element that is
     * greater than or equal to the abstraction of the set of solutions of the
     * inequality, namely the set
     * S = \{x ∈ γ([c]) | ∃ r ∈ γ([a]), ∃ s ∈ γ([b]) : r*x + s ≤ 0\}.
     * Smaller abstract elements are better.
     * Ideally, the returned abstract element is α(S).
     *)
    val inequality : t -> t -> t -> t
  end

  (**
   * [widen a b] returns the widening of [a] with [b] (i.e., [a] ∇ [b]).
   * {L {b Ensures:} [a] ⊑ ([widen a b]) and [b] ⊑ ([widen a b]).}
   *)
  val widen : t -> t -> t

  (**
   * [narrow a b] returns the narrowing of [a] with [b] (i.e., [a] ∆ [b]).
   * {L {b Requires:} [b] ⊑ [a].}
   * {L {b Ensures:} [b] ⊑ ([narrow a b]) ⊑ [a].}
   *)
  val narrow : t -> t -> t
end
