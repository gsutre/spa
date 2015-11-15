(* $Id: TestLattice.ml 3379 2015-11-15 16:05:22Z sutre $ *)


(*
 * Generic test cases for lattices.
 *)


module type I =
sig
  module L : Lattice.S
  val elements : L.t list
  val relation : (L.t * L.t) list
end

module Make (M : I) =
struct
  open TestCore
  open M

  (* Check the precondition that elements are physically distinct. *)
  let () =
    let rec distincts =
      function
      | [] -> true
      | x::l -> (not (List.memq x l)) && distincts l
    in
    assert (distincts elements)

  (* Check the precondition that relation is contained in E × E. *)
  let () =
    let pair_of_elements (x, y) =
      List.memq x elements && List.memq y elements
    in
    assert (List.for_all pair_of_elements relation)

  (* Expected equality and partial order on E. *)
  module Expect =
  struct
    (* Equality on E (physical equality). *)
    let equal x y = x == y

    (* Structural extension of equality to pairs of elements. *)
    let pair_equal (x, y) (z, t) = (equal x z) && (equal y t)

    (* Membership of a pair of elements in a relation. *)
    let pair_mem p rel = List.exists (pair_equal p) rel

    (* Transitive closure of an arbitrary relation given as a list of pairs. *)
    let rec transitive_closure rel =
      let relrel =
        List.fold_left
          (fun l (x, y) ->
           List.fold_left
             (fun l (z, t) -> if equal y z then (x, t) :: l else l)
             l
             rel)
          []
          rel
      in
      let new_pairs =
        List.filter (fun p -> not (pair_mem p rel)) relrel
      in
      if new_pairs = [] then
        rel
      else
        transitive_closure (new_pairs @ rel)

    (* Compute the transitive closure of the relation. *)
    let relation_plus = transitive_closure relation

    (* Partial order on E (reflexive-transitive closure of the relation). *)
    let leq x y =
      equal x y || pair_mem (x, y) relation_plus
  end

  (*
   * Test cases.
   *)

  (* Helper function to create test messages. *)
  let message name a b =
    Format.asprintf "@[<h>%s@ %a@ %a@]" name L.print a L.print b

  (* Helper function to call a given function for all elements. *)
  let for_all_elements f =
    List.iter f elements

  (* Helper function to call a given function for all pairs of elements. *)
  let for_all_pairs_of_elements f =
    List.iter (fun a -> List.iter (fun b -> f a b) elements) elements

  let test_equal () =
    let aux a b =
      assert_bool ~msg:(message "equal" a b) (Expect.equal a b) (L.equal a b)
    in
    for_all_pairs_of_elements aux

  let test_leq () =
    let aux a b =
      assert_bool ~msg:(message "leq" a b) (Expect.leq a b) (L.leq a b)
    in
    for_all_pairs_of_elements aux

  let test_leq_antisymmetry () =
    let aux a b =
      if L.leq a b && L.leq b a then
        assert_bool ~msg:(message "equal" a b) true (L.equal a b)
    in
    for_all_pairs_of_elements aux

  let test_bot () =
    let aux a =
      assert_bool ~msg:(message "leq" L.bot a) true (L.leq L.bot a)
    in
    for_all_elements aux

  let test_top () =
    let aux a =
      assert_bool ~msg:(message "leq" a L.top) true (L.leq a L.top)
    in
    for_all_elements aux

  let test_glb_lowerbound () =
    let aux a b =
      let glb = L.glb a b
      in
      assert_bool ~msg:(message "leq" glb a) true (L.leq glb a) ;
      assert_bool ~msg:(message "leq" glb b) true (L.leq glb b)
    in
    for_all_pairs_of_elements aux

  let test_glb_greatest () =
    let aux a b =
      let glb = L.glb a b
      in
      for_all_elements
        (fun c ->
         if L.leq c a && L.leq c b then
           assert_bool ~msg:(message "leq" c glb) true (L.leq c glb))
    in
    for_all_pairs_of_elements aux

  let test_lub_upperbound () =
    let aux a b =
      let lub = L.lub a b
      in
      assert_bool ~msg:(message "leq" a lub) true (L.leq a lub) ;
      assert_bool ~msg:(message "leq" b lub) true (L.leq b lub)
    in
    for_all_pairs_of_elements aux

  let test_lub_least () =
    let aux a b =
      let lub = L.lub a b
      in
      for_all_elements
        (fun c ->
         if L.leq a c && L.leq b c then
           assert_bool ~msg:(message "leq" lub c) true (L.leq lub c))
    in
    for_all_pairs_of_elements aux

  let tests prefix =
    [
      prefix ^ ".equal", test_equal ;
      prefix ^ ".leq", test_leq ;
      prefix ^ ".leq.antisymmetry", test_leq_antisymmetry ;
      prefix ^ ".bot", test_bot ;
      prefix ^ ".top", test_top ;
      prefix ^ ".glb.lowerbound", test_glb_lowerbound ;
      prefix ^ ".glb.greatest", test_glb_greatest ;
      prefix ^ ".lub.upperbound", test_lub_upperbound ;
      prefix ^ ".lub.least", test_lub_least ;
    ]
end
