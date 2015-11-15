(* $Id: RoundRobin.ml 3369 2015-11-13 13:06:25Z sutre $ *)


(*
 * Least fixpoint computation through round-robin iteration.
 *)


module Make (G : DiGraph.S) (L : Lattice.S) =
struct
  module Table =
  struct
    module H = Hashtbl.Make (G.Node)

    (*
     * A table is a pair (d, h) where:
     *
     * - d is a list of distinct nodes giving the domain of the table, and
     * - h is a hash table mapping nodes to facts (elements of the lattice)
     *
     * The domain d remains constant during the table's lifetime.
     *)
    type t = G.Node.t list * L.t H.t

    let print fmt (d, h) =
      let print_node fmt n =
        Format.fprintf
          fmt "@[%a@ :@ %a@]"
          G.Node.print n L.print (H.find h n)
      in
      Format.fprintf
        fmt "%a"
        (Print.list_printer_from_printer "@ " print_node)
        d

    (* Checks whether a list contains a given node (for assertions only). *)
    let contains d n =
      List.exists (G.Node.equal n) d

    (* Checks whether a list contains distinct nodes (for assertions only). *)
    let rec distincts d =
      match d with
      | [] -> true
      | n::l -> (not (contains l n)) && (distincts l)

    let create d f =
      assert (distincts d) ;
      let h = H.create (List.length d)
      in
      List.iter (fun n -> H.add h n f) d ;
      (d, h)

    let copy (d, h) = (d, H.copy h)

    let get (d, h) n =
      assert (contains d n) ;
      H.find h n

    let set (d, h) n f =
      assert (contains d n) ;
      H.replace h n f
  end

  type problem = {
    graph : G.t ;
    transfer : G.Label.t -> L.t -> L.t ;
    constant : G.Node.t -> L.t ;
  }

  type control = {
    widening : L.t -> L.t -> L.t ;
    widening_delay : int ;
    narrowing : L.t -> L.t -> L.t ;
    narrowing_delay : int ;
    verbose : bool ;
  }

  (* Returns the fact deriving from the node's inbound edges. *)
  let propagate problem tbl node =
    (* Shortcuts. *)
    let graph = problem.graph
    and transfer = problem.transfer
    and constant = problem.constant
    in
    List.fold_left
      (fun f (l, p) -> L.lub f (transfer l (Table.get tbl p)))
      (constant node)
      (G.pred graph node)

  let ascend problem widen tbl node =
    (* Current fact of the node. *)
    let f = Table.get tbl node
    (* New fact coming from the node's inbound edges. *)
    and f' = propagate problem tbl node
    in
    if L.leq f' f then
      (* Do nothing *)
      false
    else
      (* Apply widening. *)
      let f' = widen f f'
      in
      (* The fact f' is strictly greater than f. *)
      assert ((L.leq f f') && not (L.leq f' f)) ;
      (* Update the table. *)
      Table.set tbl node f' ;
      true

  let solve problem param tbl =
    let change = ref true
    and count = ref 0
    and dowiden = ref L.lub
    in
    (* Propagation loop with round-robin iteration over nodes. *)
    while !change do
      if param.verbose then
        Format.printf
          "@.@[<v 3>Round-robin ascending loop %d:@,%a@]@."
          !count Table.print tbl ;
      change := false ;
      (* Use widening once the delay elapses. *)
      if !count = param.widening_delay then dowiden := param.widening ;
      (* Propagate and update the table. *)
      List.iter
        (fun node -> change := (ascend problem !dowiden tbl node) || !change)
        (G.nodes problem.graph) ;
      incr count
    done

  let descend problem narrow tbl node =
    (* Current fact of the node. *)
    let f = Table.get tbl node
    (* New fact coming from the node's inbound edges. *)
    and f' = propagate problem tbl node
    in
    if L.leq f f' then
      (* Do nothing *)
      false
    else
      begin
        (* This assertion holds because tbl is a solution. *)
        assert (L.leq f' f) ;
        (* Apply narrowing. *)
        let f' = narrow f f'
        in
        if L.leq f f' then
          (* Do nothing *)
          false
        else
          begin
            (* The fact f' is strictly lesser than f. *)
            assert ((L.leq f' f) && not (L.leq f f')) ;
            (* Update the table. *)
            Table.set tbl node f' ;
            true
          end
      end

  let refine problem param tbl =
    let change = ref true
    and count = ref 0
    and donarrow = ref (fun _ f -> f)
    in
    (* Propagation loop with round-robin iteration over nodes. *)
    while !change do
      if param.verbose then
        Format.printf
          "@.@[<v 3>Round-robin descending loop %d:@,%a@]@."
          !count Table.print tbl ;
      change := false ;
      (* Use narrowing once the delay elapses. *)
      if !count = param.narrowing_delay then donarrow := param.narrowing ;
      (* Propagate and update the table. *)
      List.iter
        (fun node -> change := (descend problem !donarrow tbl node) || !change)
        (G.nodes problem.graph) ;
      incr count
    done
end
