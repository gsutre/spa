(* $Id: Fixpoint.mli 3367 2015-11-13 09:31:53Z sutre $ *)


(**
 * Conservative Least Fixpoint Engines.
 *
 * A conservative least fixpoint engine solves monotonic dataflow problems.  The
 * latter are given by a labeled directed graph (N, Σ, E), a lattice (L, ⊑) of
 * dataflow {e facts}, a monotonic transfer function t{_ l} : L → L for each
 * label l ∈ Σ, and a constant fact c{_ n} ∈ L for each node n ∈ N.  A monotonic
 * dataflow problem induces a monotonic function F on the pointwise lifting
 * ((N → L), ⊑) of the lattice (L, ⊑).  The function F is defined as follows:
 *
 * {C F(v)(n) = ⊔\{t{_ l}(v(p)) | (p, l, n) ∈ E\} ⊔ c{_ n}}
 *
 * Equivalently, a monotonic dataflow problem induces a monotonic constraint
 * system S with variables V{_ n}, for n ∈ N, that range over L.  The constraint
 * system S contains the following constraints:
 *
 * - V{_ n} ⊒ c{_ n} for each node n ∈ N, and
 * - V{_ n} ⊒ t{_ l}(V{_ p}) for each edge (p, l, n) ∈ E.
 *
 * A solution of the constraint system S is a post-fixpoint of the function F,
 * and vice-versa.  Similarly, when they exist, the least solution of S and the
 * least fixpoint of F coincide.
 *
 * Conservative least fixpoint engines provide a data structure to manage maps
 * from N to L, as well as two functions: [solve] and [refine].  Both functions
 * take a monotonic dataflow problem P as input.  They are also given parameters
 * that control their behavior.   The function [solve] computes a solution of
 * the monotonic constraint system induced by P.  The function [refine] takes a
 * solution of this constraint system and refines it.
 *
 * {L {b Implemented by:} {! RoundRobin}.}
 *)


(**
 * Common interface to all conservative least fixpoint engine implementations.
 *)
module type S = functor (G : DiGraph.S) -> functor (L : Lattice.S) ->
sig
  (**
   * Module to manage finite maps, with {e in-place modification}, from nodes
   * (i.e., values of type [G.Node.t]) to facts (i.e., values of type [L.t]).
   * The domain of the map is specified at creation time, and remains constant
   * afterwards.
   *
   * Due to their imperative nature, these maps are hereafter called tables.
   *)
  module Table :
  sig
    (**
     * The printable type of tables.
     *)
    include Print.S

    (**
     * [create [n1; ...; nN] f] creates a new table with domain
     * \{[n1], ..., [nN]\} and such that the image of each node is the fact [f].
     * {L {b Requires:} [n1], ..., [nN] are distinct nodes.}
     *)
    val create : G.Node.t list -> L.t -> t

    (**
     * [copy tbl] returns a copy of the table [tbl].
     *)
    val copy : t -> t

    (**
     * [get tbl n] returns the image of [n] in the table [tbl].
     * {L {b Requires:} [n] is a node contained in the domain of [tbl].}
     *)
    val get : t -> G.Node.t -> L.t

    (**
     * [set tbl n f] sets the image of [n] to the fact [f] in the table [tbl].
     * {L {b Requires:} [n] is a node contained in the domain of [tbl].}
     *)
    val set : t -> G.Node.t -> L.t -> unit
  end

  (**
   * Monotonic dataflow problems are given as [problem] records.
   *)
  type problem = {
    graph : G.t ;
    transfer : G.Label.t -> L.t -> L.t ;
    constant : G.Node.t -> L.t ;
  }

  (**
   * The fixpoint engine's parameters are given as a [control] record.  The
   * [widening] and [narrowing] fields shall conform to the specification of
   * widening and narrowing operators (see {! AbstractDomain.S}).
   *)
  type control = {
    widening : L.t -> L.t -> L.t ;
    widening_delay : int ;          (** Disables widening when negative. *)
    narrowing : L.t -> L.t -> L.t ;
    narrowing_delay : int ;         (** Disables narrowing when negative. *)
    verbose : bool ;
  }

  (**
   * [solve problem param tbl] computes a solution of the constraint system S
   * induced by [problem].  The table [tbl] shall contain “necessary” facts
   * (e.g., ⊥), as specified in the requirements below.  This table is modified
   * (in-place) into a solution of S.
   * The quality of the resulting solution can be tweaked through the [control]
   * parameters [param].  Smaller solutions are better.
   * {L {b Requires:} [tbl] is pointwise lesser than or equal to every solution
   *    of S.}
   * {L {b Ensures:} [tbl] is a solution of S.}
   *)
  val solve : problem -> control -> Table.t -> unit

  (**
   * [refine problem param tbl] refines a solution of the constraint system S
   * induced by [problem].  The table [tbl] shall contain a solution of S, and
   * is modified (in-place) into a smaller (or equal) solution of S.
   * The quality of the resulting solution can be tweaked through the [control]
   * parameters [param].  Smaller solutions are better.
   * {L {b Requires:} [tbl] is a solution of S.}
   * {L {b Ensures:} [tbl] is a solution of S and is pointwise lesser than or
   *    equal to before.}
   *)
  val refine : problem -> control -> Table.t -> unit
end
