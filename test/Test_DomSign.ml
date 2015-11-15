(* $Id: Test_DomSign.ml 3371 2015-11-13 17:09:07Z sutre $ *)


(*
 * Unit tests for the DomSign module.
 *)


open TestCore
open DomSign

(* Import utility functions. *)
module TU = TestUtil.OfNumericalDomain (DomSign)
open TU

(* Elements of the sign domain (except bot and top). *)
let negative = abs (-1)
let zero = abs 0
let positive = abs 1
let nonpositive = lub negative zero
let nonzero = lub negative positive
let nonnegative = lub zero positive
let elements =
  [bot ; negative ; zero ; positive ; nonpositive ; nonzero ; nonnegative ; top]

let test_print () =
  assert_print ~msg:"print bot"         "⊥"  print bot ;
  assert_print ~msg:"print negative"    "-"  print negative ;
  assert_print ~msg:"print zero"        "0"  print zero ;
  assert_print ~msg:"print positive"    "+"  print positive ;
  assert_print ~msg:"print nonpositive" "-0" print nonpositive ;
  assert_print ~msg:"print nonzero"     "-+" print nonzero ;
  assert_print ~msg:"print nonnegative" "0+" print nonnegative ;
  assert_print ~msg:"print top"         "⊤"  print top

let test_abs () =
  assert_equal_t ~msg:"abs -13" "-" (abs (-13)) ;
  assert_equal_t ~msg:"abs 0"   "0" (abs 0) ;
  assert_equal_t ~msg:"abs 7"   "+" (abs 7)

let test_empty () =
  assert_bool ~msg:"empty bot"      true  (empty bot) ;
  assert_bool ~msg:"empty negative" false (empty negative) ;
  assert_bool ~msg:"empty zero"     false (empty zero) ;
  assert_bool ~msg:"empty nonzero"  false (empty nonzero) ;
  assert_bool ~msg:"empty top"      false (empty top)

let test_add () =
  let aux a b expected =
    assert_equal_t ~msg:(message "add" a b) expected (Op.add a b) ;
    if not (a == b) then
      assert_equal_t ~msg:(message "add" b a) expected (Op.add b a)
  in
  List.iter (fun a -> aux bot a "⊥") elements ;
  aux negative negative "-" ;
  aux negative positive "⊤" ;
  aux negative nonnegative "⊤" ;
  aux zero zero "0" ;
  aux zero positive "+" ;
  aux zero nonnegative "0+" ;
  aux zero nonzero "-+" ;
  aux zero top "⊤" ;
  aux positive positive "+" ;
  aux positive nonpositive "⊤" ;
  aux nonpositive nonpositive "-0" ;
  aux nonpositive nonnegative "⊤" ;
  aux nonzero negative "⊤" ;
  aux nonzero nonpositive "⊤" ;
  aux nonnegative nonnegative "0+" ;
  aux top negative "⊤" ;
  aux top nonpositive "⊤" ;
  aux top top "⊤"

let opposite a =
  match a with
  | "⊥" | "⊤" | "0" | "-+" -> a
  | "-" -> "+"
  | "+" -> "-"
  | "-0" -> "0+"
  | "0+" -> "-0"
  | _ -> invalid_arg "Test_DomSign.opposite"

let test_sub () =
  let aux a b expected =
    assert_equal_t ~msg:(message "sub" a b) expected (Op.sub a b) ;
    if not (a == b) then
      assert_equal_t ~msg:(message "sub" b a) (opposite expected) (Op.sub b a)
  in
  List.iter (fun a -> aux bot a "⊥") elements ;
  aux negative negative "⊤" ;
  aux negative positive "-" ;
  aux negative nonnegative "-" ;
  aux zero zero "0" ;
  aux zero positive "-" ;
  aux zero nonnegative "-0" ;
  aux zero nonzero "-+" ;
  aux zero top "⊤" ;
  aux positive positive "⊤" ;
  aux positive nonpositive "+" ;
  aux nonpositive nonpositive "⊤" ;
  aux nonpositive nonnegative "-0" ;
  aux nonzero negative "⊤" ;
  aux nonzero nonpositive "⊤" ;
  aux nonnegative nonnegative "⊤" ;
  aux top negative "⊤" ;
  aux top nonpositive "⊤" ;
  aux top top "⊤"

let test_mul () =
  let aux a b expected =
    assert_equal_t ~msg:(message "mul" a b) expected (Op.mul a b) ;
    if not (a == b) then
      assert_equal_t ~msg:(message "mul" b a) expected (Op.mul b a)
  in
  List.iter (fun a -> aux bot a "⊥") elements ;
  aux negative negative "+" ;
  aux negative positive "-" ;
  aux negative nonnegative "-0" ;
  aux zero zero "0" ;
  aux zero positive "0" ;
  aux zero nonnegative "0" ;
  aux zero nonzero "0" ;
  aux zero top "0" ;
  aux positive positive "+" ;
  aux positive nonpositive "-0" ;
  aux nonpositive nonpositive "0+" ;
  aux nonpositive nonnegative "-0" ;
  aux nonzero negative "-+" ;
  aux nonzero nonpositive "⊤" ;
  aux nonnegative nonnegative "0+" ;
  aux top negative "⊤" ;
  aux top nonpositive "⊤" ;
  aux top top "⊤"

let inverse a =
  match a with
  | "⊥" | "⊤" -> a
  | "-" | "-0" -> "-0"
  | "0" -> "⊥"
  | "+" | "0+" -> "0+"
  | "-+" -> "⊤"
  | _ -> invalid_arg "Test_DomSign.inverse"

let test_div () =
  let aux a b expected =
    assert_equal_t ~msg:(message "div" a b) expected (Op.div a b) ;
    if not (a == b) then
      assert_equal_t ~msg:(message "div" b a) (inverse expected) (Op.div b a)
  in
  List.iter (fun a -> aux bot a "⊥") elements ;
  aux negative negative "0+" ;
  aux negative positive "-0" ;
  aux negative nonnegative "-0" ;
  aux zero zero "⊥" ;
  aux zero positive "0" ;
  aux zero nonnegative "0" ;
  aux zero nonzero "0" ;
  aux zero top "0" ;
  aux positive positive "0+" ;
  aux positive nonpositive "-0" ;
  aux nonpositive nonpositive "0+" ;
  aux nonpositive nonnegative "-0" ;
  aux nonzero negative "⊤" ;
  aux nonzero nonpositive "⊤" ;
  aux nonnegative nonnegative "0+" ;
  aux top negative "⊤" ;
  aux top nonpositive "⊤" ;
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
  List.iter (fun a -> aux' top top a a) elements ;
  aux negative negative nonnegative "⊥" ;
  aux negative negative zero "⊥" ;
  aux negative negative nonpositive "-" ;
  aux negative negative top "-" ;
  aux negative zero negative "⊥" ;
  aux negative zero zero "0" ;
  aux negative zero positive "⊥" ;
  aux negative zero top "0" ;
  aux negative positive nonnegative "+" ;
  aux negative positive zero "⊥" ;
  aux negative positive nonpositive "⊥" ;
  aux negative positive top "+" ;
  aux negative nonzero negative "-" ;
  aux negative nonzero zero "⊥" ;
  aux negative nonzero positive "+" ;
  aux negative nonzero top "-+" ;
  aux zero zero zero "0" ;
  aux zero zero nonzero "-+" ;
  aux zero zero top "⊤" ;
  aux zero nonzero zero "⊥" ;
  aux zero nonzero nonzero "⊥" ;
  aux zero nonzero top "⊥" ;
  aux nonnegative negative nonnegative "+" ;
  aux nonnegative negative zero "⊥" ;
  aux nonnegative negative nonpositive "⊥" ;
  aux nonnegative negative top "+" ;
  aux nonnegative zero negative "-" ;
  aux nonnegative zero zero "0" ;
  aux nonnegative zero positive "+" ;
  aux nonnegative zero top "⊤" ;
  aux nonnegative positive nonnegative "⊥" ;
  aux nonnegative positive zero "⊥" ;
  aux nonnegative positive nonpositive "-" ;
  aux nonnegative positive top "-" ;
  aux nonnegative nonzero negative "-" ;
  aux nonnegative nonzero zero "⊥" ;
  aux nonnegative nonzero positive "+" ;
  aux nonnegative nonzero top "-+" ;
  aux nonzero zero zero "0" ;
  aux nonzero zero nonzero "⊥" ;
  aux nonzero zero top "0" ;
  aux nonzero nonzero zero "⊥" ;
  aux nonzero nonzero nonzero "-+" ;
  aux nonzero nonzero top "-+" ;
  aux top zero zero "0" ;
  aux top zero nonzero "-+" ;
  aux top zero top "⊤" ;
  aux top nonzero zero "⊥" ;
  aux top nonzero nonzero "-+" ;
  aux top nonzero top "-+"

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
  List.iter (fun a -> aux' top top a a) elements ;
  aux negative negative nonnegative "0+" ;
  aux negative negative zero "0" ;
  aux negative negative nonpositive "-0" ;
  aux negative negative top "⊤" ;
  aux negative zero negative "⊥" ;
  aux negative zero zero "0" ;
  aux negative zero positive "+" ;
  aux negative zero top "0+" ;
  aux negative positive nonnegative "+" ;
  aux negative positive zero "⊥" ;
  aux negative positive nonpositive "⊥" ;
  aux negative positive top "+" ;
  aux negative nonzero negative "-" ;
  aux negative nonzero zero "0" ;
  aux negative nonzero positive "+" ;
  aux negative nonzero top "⊤" ;
  aux zero zero zero "0" ;
  aux zero zero nonzero "-+" ;
  aux zero zero top "⊤" ;
  aux zero nonzero zero "0" ;
  aux zero nonzero nonzero "-+" ;
  aux zero nonzero top "⊤" ;
  aux nonnegative negative nonnegative "0+" ;
  aux nonnegative negative zero "0" ;
  aux nonnegative negative nonpositive "-0" ;
  aux nonnegative negative top "⊤" ;
  aux nonnegative zero negative "-" ;
  aux nonnegative zero zero "0" ;
  aux nonnegative zero positive "+" ;
  aux nonnegative zero top "⊤" ;
  aux nonnegative positive nonnegative "⊥" ;
  aux nonnegative positive zero "⊥" ;
  aux nonnegative positive nonpositive "-" ;
  aux nonnegative positive top "-" ;
  aux nonnegative nonzero negative "-" ;
  aux nonnegative nonzero zero "0" ;
  aux nonnegative nonzero positive "+" ;
  aux nonnegative nonzero top "⊤" ;
  aux nonzero zero zero "0" ;
  aux nonzero zero nonzero "-+" ;
  aux nonzero zero top "⊤" ;
  aux nonzero nonzero zero "0" ;
  aux nonzero nonzero nonzero "-+" ;
  aux nonzero nonzero top "⊤" ;
  aux top zero zero "0" ;
  aux top zero nonzero "-+" ;
  aux top zero top "⊤" ;
  aux top nonzero zero "0" ;
  aux top nonzero nonzero "-+" ;
  aux top nonzero top "⊤"

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
  module L = DomSign
  let elements = elements
  let relation =
    [
      (bot, negative) ;
      (bot, zero) ;
      (bot, positive) ;
      (negative, nonpositive) ;
      (negative, nonzero) ;
      (zero, nonpositive) ;
      (zero, nonnegative) ;
      (positive, nonzero) ;
      (positive, nonnegative) ;
      (nonpositive, top) ;
      (nonzero, top) ;
      (nonnegative, top) ;
    ]
end

module T = TestLattice.Make (M)

(* Collection of all tests. *)
let alltests =
  [
    "domsign.print", test_print ;
  ]
  @
  (T.tests "domsign")
  @
  [
    "domsign.abs", test_abs ;
    "domsign.empty", test_empty ;
    "domsign.add", test_add ;
    "domsign.sub", test_sub ;
    "domsign.mul", test_mul ;
    "domsign.div", test_div ;
    "domsign.equality", test_equality ;
    "domsign.inequality", test_inequality ;
    "domsign.widen", test_widen ;
    "domsign.narrow", test_narrow ;
  ]

(* This test suite. *)
let suite = ("DomSign", alltests)
