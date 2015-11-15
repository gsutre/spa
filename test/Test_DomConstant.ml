(* $Id: Test_DomConstant.ml 3373 2015-11-13 17:29:13Z sutre $ *)


(*
 * Unit tests for the DomConstant module.
 *)


open TestCore
open DomConstant

(* Import utility functions. *)
module TU = TestUtil.OfNumericalDomain (DomConstant)
open TU

(* Define some elements of the constant domain (except bot and top). *)
let minus_five = abs (-5)
let minus_three = abs (-3)
let zero = abs 0
let plus_three = abs 3
let plus_five = abs 5
let elements =
  [bot ; minus_five ; minus_three ; zero ; plus_three ; plus_five ; top]

let test_print () =
  assert_print ~msg:"print bot"         "⊥"  print bot ;
  assert_print ~msg:"print minus_five"  "-5" print minus_five ;
  assert_print ~msg:"print minus_three" "-3" print minus_three ;
  assert_print ~msg:"print zero"        "0"  print zero ;
  assert_print ~msg:"print plus_three"  "3"  print plus_three ;
  assert_print ~msg:"print plus_five"   "5"  print plus_five ;
  assert_print ~msg:"print top"         "⊤"  print top

let test_abs () =
  assert_equal_t ~msg:"abs -13" "-13" (abs (-13)) ;
  assert_equal_t ~msg:"abs 0"   "0"   (abs 0) ;
  assert_equal_t ~msg:"abs 7"   "7"   (abs 7)

let test_empty () =
  assert_bool ~msg:"empty bot"        true  (empty bot) ;
  assert_bool ~msg:"empty minus_five" false (empty minus_five) ;
  assert_bool ~msg:"empty zero"       false (empty zero) ;
  assert_bool ~msg:"empty plus_three" false (empty plus_three) ;
  assert_bool ~msg:"empty top"        false (empty top)

let test_add () =
  let aux a b expected =
    assert_equal_t ~msg:(message "add" a b) expected (Op.add a b) ;
    if not (a == b) then
      assert_equal_t ~msg:(message "add" b a) expected (Op.add b a)
  in
  List.iter (fun a -> aux bot a "⊥") elements ;
  aux minus_five minus_five "-10" ;
  aux minus_five plus_three "-2" ;
  aux zero minus_five "-5" ;
  aux zero zero "0" ;
  aux zero plus_three "3" ;
  aux zero top "⊤" ;
  aux plus_three plus_three "6" ;
  aux top minus_five "⊤" ;
  aux top plus_three "⊤" ;
  aux top top "⊤"

let opposite a =
  match a with
  | "⊥" | "⊤" | "0" -> a
  | "-8" -> "8"
  | "-5" -> "5"
  | "-3" -> "3"
  | "3" -> "-3"
  | "5" -> "-5"
  | "8" -> "-8"
  | _ -> invalid_arg "Test_DomConstant.opposite"

let test_sub () =
  let aux a b expected =
    assert_equal_t ~msg:(message "sub" a b) expected (Op.sub a b) ;
    if not (a == b) then
      assert_equal_t ~msg:(message "sub" b a) (opposite expected) (Op.sub b a)
  in
  List.iter (fun a -> aux bot a "⊥") elements ;
  aux minus_five minus_five "0" ;
  aux minus_five plus_three "-8" ;
  aux zero minus_five "5" ;
  aux zero zero "0" ;
  aux zero plus_three "-3" ;
  aux zero top "⊤" ;
  aux plus_three plus_three "0" ;
  aux top minus_five "⊤" ;
  aux top plus_three "⊤" ;
  aux top top "⊤"

let test_mul () =
  let aux a b expected =
    assert_equal_t ~msg:(message "mul" a b) expected (Op.mul a b) ;
    if not (a == b) then
      assert_equal_t ~msg:(message "mul" b a) expected (Op.mul b a)
  in
  List.iter (fun a -> aux bot a "⊥") elements ;
  aux minus_five minus_five "25" ;
  aux minus_five plus_three "-15" ;
  aux zero minus_five "0" ;
  aux zero zero "0" ;
  aux zero plus_three "0" ;
  aux zero top "0" ;
  aux plus_three plus_three "9" ;
  aux top minus_five "⊤" ;
  aux top plus_three "⊤" ;
  aux top top "⊤"

let test_div () =
  let aux a b expected =
    assert_equal_t ~msg:(message "div" a b) expected (Op.div a b)
  in
  List.iter (fun a -> aux bot a "⊥") elements ;
  aux minus_five minus_five "1" ;
  aux minus_five zero "⊥" ;
  aux minus_five plus_three "-1" ;
  aux minus_five top "⊤" ;
  aux (abs (-13)) plus_three "-4" ;
  aux zero minus_five "0" ;
  aux zero zero "⊥" ;
  aux zero plus_three "0" ;
  aux zero top "0" ;
  aux plus_three minus_five "0" ;
  aux plus_three zero "⊥" ;
  aux plus_three plus_three "1" ;
  aux plus_three top "⊤" ;
  aux (abs (17)) minus_five "-3" ;
  aux top minus_five "⊤" ;
  aux top zero "⊥" ;
  aux top plus_three "⊤" ;
  aux top top "⊤"

let test_equality () =
  let aux a b c expected =
    assert_equal_t ~msg:(message' "equality" a b c) expected (Op.equality a b c)
  and aux' a b c expected =
    assert_equal
      ~equal ~print
      ~msg:(message' "equality" a b c) expected (Op.equality a b c)
  in
  List.iter (fun a -> aux bot top a "⊥" ; aux bot a top "⊥") elements ;
  List.iter (fun a -> aux top bot a "⊥" ; aux a bot top "⊥") elements ;
  List.iter (fun a -> aux top a bot "⊥" ; aux a top bot "⊥") elements ;
  List.iter (fun a -> if not (empty a) then aux a top top "⊤") elements ;
  List.iter (fun a -> if not (empty a) then aux top a top "⊤") elements ;
  List.iter (fun a -> aux' top top a a) elements ;
  aux minus_five minus_three zero "⊥" ;
  aux minus_five minus_three top "⊥" ;
  aux minus_five zero zero "0" ;
  aux minus_five zero plus_five "⊥" ;
  aux minus_five zero top "0" ;
  aux zero minus_three plus_three "⊥" ;
  aux zero minus_three top "⊥" ;
  aux zero zero zero "0" ;
  aux zero zero plus_five "5" ;
  aux zero zero top "⊤" ;
  aux zero plus_three minus_three "⊥" ;
  aux zero plus_three top "⊥" ;
  aux plus_three minus_three zero "⊥" ;
  aux plus_three minus_three top "1" ;
  aux plus_three zero zero "0" ;
  aux plus_three zero plus_five "⊥" ;
  aux plus_three zero top "0" ;
  aux top minus_three zero "⊥" ;
  aux top zero zero "0" ;
  aux top zero plus_five "5"

let test_inequality () =
  let aux a b c expected =
    assert_equal_t
      ~msg:(message' "inequality" a b c) expected (Op.inequality a b c)
  and aux' a b c expected =
    assert_equal
      ~equal ~print
      ~msg:(message' "inequality" a b c) expected (Op.inequality a b c)
  in
  List.iter (fun a -> aux bot top a "⊥" ; aux bot a top "⊥") elements ;
  List.iter (fun a -> aux top bot a "⊥" ; aux a bot top "⊥") elements ;
  List.iter (fun a -> aux top a bot "⊥" ; aux a top bot "⊥") elements ;
  List.iter (fun a -> if not (empty a) then aux a top top "⊤") elements ;
  List.iter (fun a -> if not (empty a) then aux top a top "⊤") elements ;
  List.iter (fun a -> aux' top top a a) elements ;
  aux minus_five minus_three zero "0" ;
  aux minus_five minus_three top "⊤" ;
  aux minus_five zero zero "0" ;
  aux minus_five zero plus_five "5" ;
  aux minus_five zero top "⊤" ;
  aux zero minus_three plus_three "3" ;
  aux zero minus_three top "⊤" ;
  aux zero zero zero "0" ;
  aux zero zero plus_five "5" ;
  aux zero zero top "⊤" ;
  aux zero plus_three minus_three "⊥" ;
  aux zero plus_three top "⊥" ;
  aux plus_three minus_three zero "0" ;
  aux plus_three minus_three top "⊤" ;
  aux plus_three zero zero "0" ;
  aux plus_three zero plus_five "⊥" ;
  aux plus_three zero top "⊤" ;
  aux top minus_three zero "0" ;
  aux top zero zero "0" ;
  aux top zero plus_five "5"

(* The widening should coincide with the least upper bound. *)
let test_widen () =
  let aux a b =
    assert_equal ~equal ~print ~msg:(message "widen" a b) (lub a b) (widen a b)
  in
  List.iter (fun a -> List.iter (fun b -> aux a b) elements) elements

(* The narrowing should simply return its second argument. *)
let test_narrow () =
  let aux a b =
    if leq b a then
      assert_equal ~equal ~print ~msg:(message "narrow" a b) b (narrow a b)
  in
  List.iter (fun a -> List.iter (fun b -> aux a b) elements) elements

(* Create the generic lattice test cases. *)
module M =
struct
  module L = DomConstant
  let elements = elements
  let relation =
    [
      (bot, minus_five) ;
      (bot, minus_three) ;
      (bot, zero) ;
      (bot, plus_three) ;
      (bot, plus_five) ;
      (minus_five, top) ;
      (minus_three, top) ;
      (zero, top) ;
      (plus_three, top) ;
      (plus_five, top) ;
    ]
end

module T = TestLattice.Make (M)

(* Collection of all tests. *)
let alltests =
  [
    "domconstant.print", test_print ;
  ]
  @
  (T.tests "domconstant")
  @
  [
    "domconstant.abs", test_abs ;
    "domconstant.empty", test_empty ;
    "domconstant.add", test_add ;
    "domconstant.sub", test_sub ;
    "domconstant.mul", test_mul ;
    "domconstant.div", test_div ;
    "domconstant.equality", test_equality ;
    "domconstant.inequality", test_inequality ;
    "domconstant.widen", test_widen ;
    "domconstant.narrow", test_narrow ;
  ]

(* This test suite. *)
let suite = ("DomConstant", alltests)
