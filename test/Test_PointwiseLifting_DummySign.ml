(* $Id: Test_PointwiseLifting_DummySign.ml 3369 2015-11-13 13:06:25Z sutre $ *)


(*
 * Unit tests for the PointwiseLifting module (instantiated with DummySign).
 *
 * Checks the soundness of abstract semantics operators when γ(⊥) ≠ ∅ and
 * with coarse abstract zero and subtraction.
 *)


open TestCore

(* Dummy sign numerical non-relational abstract domain. *)
module DummySign =
struct
  type t = bool * bool

  let print fmt =
    function
    | (false, false) -> Format.pp_print_string fmt "C" (* [-1,  1] *)
    | (true , false) -> Format.pp_print_string fmt "L" (* [-∞,  1] *)
    | (false, true ) -> Format.pp_print_string fmt "R" (* [-1, +∞] *)
    | (true , true ) -> Format.pp_print_string fmt "A" (* [-∞, +∞] *)

  let bot = (false, false)
  let top = (true , true )

  let equal = (=)
  let leq (l1, r1) (l2, r2) = (l1 <= l2) && (r1 <= r2)
  let glb (l1, r1) (l2, r2) = (min l1 l2, min r1 r2)
  let lub (l1, r1) (l2, r2) = (max l1 l2, max r1 r2)
  let abs x = (x < -1, x > 1)
  let empty _ = false

  module Op =
  struct
    let add _ _ = top
    let sub _ _ = top
    let mul a b = if equal a bot && equal b bot then bot else top
    let div a _ = if equal a bot then bot else top
    let equality _ _ c = c
    let inequality _ _ c = c
  end

  let widen = lub
  let narrow a b = assert (leq b a) ; b
end

(* Create the pointwise lifting of the dummy sign domain. *)
module PL = PointwiseLifting.Make (DummySign)
open PL

(* Import utility functions. *)
module TU = TestUtil.OfAbstractDomain (PL)
open TU

let test_print () =
  assert_print ~msg:"print bot" "{C}" print bot ;
  assert_print ~msg:"print top" "{A}" print top

let test_post_bot () =
  (* Shortcuts. *)
  let sequence = sequence post "post" bot
  and x = var "x"
  and y = var "y"
  in
  sequence
    [x := cst (-3) ; x := cst 0 ; x := cst 5 ; y := cst (-7) ; y := cst 0]
    ["{C, x ↦ L}" ; "{C}" ; "{C, x ↦ R}" ; "{C, x ↦ R, y ↦ L}" ; "{C, x ↦ R}"] ;
  sequence
    [x := x + y ; x := y ; x := x - y ; y := x * y ; y := (cst 0) / x]
    ["{C, x ↦ A}" ; "{C}" ; "{C, x ↦ A}" ; "{C, x ↦ A, y ↦ A}" ; "{C, x ↦ A}"] ;
  sequence
    [x == cst (-3) ; x <= cst 0 ; x >= cst 5 ; y <= cst 0 ; y > cst 7]
    ["{C}" ; "{C}" ; "{C}" ; "{C}" ; "{C}"] ;
  sequence
    [x == y ; x <= y ; x > x - y ; y <= x * y ; x / y > cst 0]
    ["{C}" ; "{C}" ; "{C}" ; "{C}" ; "{C}"]

let test_post_top () =
  (* Shortcuts. *)
  let sequence = sequence post "post" top
  and x = var "x"
  and y = var "y"
  in
  sequence
    [x := cst (-3) ; x := cst 0 ; x := cst 5 ; y := cst (-7) ; y := cst 0]
    ["{A, x ↦ L}" ; "{A, x ↦ C}" ; "{A, x ↦ R}" ; "{A, x ↦ R, y ↦ L}" ;
     "{A, x ↦ R, y ↦ C}"] ;
  sequence
    [x := x + y ; x := y ; x := x - y ; y := x * y ; y := (cst 0) / x]
    ["{A}" ; "{A}" ; "{A}" ; "{A}" ; "{A, y ↦ C}"] ;
  sequence
    [x == cst (-3) ; x <= cst 0 ; x >= cst 5 ; y <= cst 0 ; y > cst 7]
    ["{A}" ; "{A}" ; "{A}" ; "{A}" ; "{A}"] ;
  sequence
    [x == y ; x <= y ; x > x - y ; y <= x * y ; x / y > cst 0]
    ["{A}" ; "{A}" ; "{A}" ; "{A}" ; "{A}"]

let test_pre_bot () =
  (* Shortcuts. *)
  let sequence = sequence pre "pre" bot
  and x = var "x"
  and y = var "y"
  in
  sequence
    [x := cst (-3) ; x := cst 0 ; x := cst 5 ; y := cst (-7) ; y := cst 0]
    ["{C, x ↦ A}" ; "{C, x ↦ A}" ; "{C, x ↦ A}" ; "{C, x ↦ A, y ↦ A}" ;
     "{C, x ↦ A, y ↦ A}"] ;
  sequence
    [x := x + y ; x := y ; x := x - y ; y := x * y ; y := (cst 0) / x]
    ["{C, x ↦ A}" ; "{C, x ↦ A}" ; "{C, x ↦ A}" ; "{C, x ↦ A, y ↦ A}" ;
     "{C, x ↦ A, y ↦ A}"] ;
  sequence
    [x == cst (-3) ; x <= cst 0 ; x >= cst 5 ; y <= cst 0 ; y > cst 7]
    ["{C}" ; "{C}" ; "{C}" ; "{C}" ; "{C}"] ;
  sequence
    [x == y ; x <= y ; x > x - y ; y <= x * y ; x / y > cst 0]
    ["{C}" ; "{C}" ; "{C}" ; "{C}" ; "{C}"]

let test_pre_top () =
  (* Shortcuts. *)
  let sequence = sequence pre "pre" top
  and x = var "x"
  and y = var "y"
  in
  sequence
    [x := cst (-3) ; x := cst 0 ; x := cst 5 ; y := cst (-7) ; y := cst 0]
    ["{A}" ; "{A}" ; "{A}" ; "{A}" ; "{A}"] ;
  sequence
    [x := x + y ; x := y ; x := x - y ; y := x * y ; y := (cst 0) / x]
    ["{A}" ; "{A}" ; "{A}" ; "{A}" ; "{A}"] ;
  sequence
    [x == cst (-3) ; x <= cst 0 ; x >= cst 5 ; y <= cst 0 ; y > cst 7]
    ["{A}" ; "{A}" ; "{A}" ; "{A}" ; "{A}"] ;
  sequence
    [x == y ; x <= y ; x > x - y ; y <= x * y ; x / y > cst 0]
    ["{A}" ; "{A}" ; "{A}" ; "{A}" ; "{A}"]

(* Collection of all tests. *)
let alltests =
  [
    "pwlifting.print", test_print ;
    "pwlifting.post.bot", test_post_bot ;
    "pwlifting.post.top", test_post_top ;
    "pwlifting.pre.bot", test_pre_bot ;
    "pwlifting.pre.top", test_pre_top ;
  ]

(* This test suite. *)
let suite = ("PointwiseLifting of DummySign", alltests)
