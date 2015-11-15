(* $Id: DiGraph.mli 3251 2015-10-29 15:56:15Z sutre $ *)


(**
 * Labeled Directed Graphs.
 *
 * A {e labeled directed graph} is a triple (N, Σ, E) where N is a set of
 * {e nodes}, Σ is a set of {e labels} and E ⊆ N×Σ×N is a set of {e edges}.
 * This module only specifies a “read-only” interface for labeled directed
 * graphs.
 *
 * Nodes are totally ordered and hashable.
 *
 * {L {b Implemented by:} {! Automaton}.}
 *
 * @see <http://en.wikipedia.org/wiki/Directed_graph>
 *   Wikipedia's article on directed graphs
 *)


(**
 * Common interface to all labeled directed graph implementations.
 *)
module type S =
sig
  (**
   * The printable type of labeled directed graphs.
   *)
  include Print.S

  (**
   * Module to manage nodes of labeled directed graphs.
   * Nodes are totally ordered and hashable.
   *
   * @see <http://caml.inria.fr/pub/docs/manual-ocaml/libref/Set.OrderedType.html>
   *   Set.OrderedType
   * @see <http://caml.inria.fr/pub/docs/manual-ocaml/libref/Hashtbl.HashedType.html>
   *   Hashtbl.HashedType
   *)
  module Node :
  sig
    (**
     * The printable type of nodes.
     *)
    include Print.S

    (**
     * Total ordering on nodes.
     *)
    val compare : t -> t -> int

    (**
     * Equality on nodes.
     *)
    val equal : t -> t -> bool

    (**
     * Hashing on nodes.
     *)
    val hash : t -> int
  end

  (**
   * Module to manage edge labels.
   *)
  module Label : Print.S

  (**
   * [nodes g] returns the nodes of the labeled directed graph [g], as a list.
   * The order in which the nodes appear in the list is unspecified.
   *)
  val nodes : t -> Node.t list

  (**
   * [succ g n] returns the {e outbound} edges of [n] in the labeled directed
   * graph [g].
   * {L {b Requires:} [n] is a node contained in [g].}
   *)
  val succ : t -> Node.t -> (Label.t * Node.t) list

  (**
   * [pred g n] returns the {e inbound} edges of [n] in the labeled directed
   * graph [g].
   * {L {b Requires:} [n] is a node contained in [g].}
   *)
  val pred : t -> Node.t -> (Label.t * Node.t) list
end
