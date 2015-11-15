(* $Id: Test_DomInterval.ml 3376 2015-11-14 23:22:54Z sutre $ *)


(*
 * Unit tests for the DomInterval module.
 *)


open TestCore
open DomInterval

(* Import utility functions. *)
module TU = TestUtil.OfNumericalDomain (DomInterval)
open TU

(* Define some elements of the interval domain (except bot and top). *)
let m3 = abs (-3)
let zero = abs 0
let p5 = abs 5
let m3_to_p5 = lub m3 p5
let inf_to_m3 = Op.inequality (abs 1) (abs 3) top
let inf_to_p5 = Op.inequality (abs 1) (abs (-5)) top
let m3_to_inf = Op.inequality (abs (-1)) m3 top
let p5_to_inf = Op.inequality (abs (-1)) p5 top
let elements =
  [bot ; m3 ; zero ; p5 ; m3_to_p5 ; inf_to_m3 ; inf_to_p5 ; m3_to_inf ;
   p5_to_inf ; top]

let test_print () =
  assert_print ~msg:"print bot"       "⊥"        print bot ;
  assert_print ~msg:"print m3"        "[-3, -3]" print m3 ;
  assert_print ~msg:"print zero"      "[0, 0]"   print zero ;
  assert_print ~msg:"print p5"        "[5, 5]"   print p5 ;
  assert_print ~msg:"print m3_to_p5"  "[-3, 5]"  print m3_to_p5 ;
  assert_print ~msg:"print inf_to_m3" "[-∞, -3]" print inf_to_m3 ;
  assert_print ~msg:"print inf_to_p5" "[-∞, 5]"  print inf_to_p5 ;
  assert_print ~msg:"print m3_to_inf" "[-3, +∞]" print m3_to_inf ;
  assert_print ~msg:"print p5_to_inf" "[5, +∞]"  print p5_to_inf ;
  assert_print ~msg:"print top"       "[-∞, +∞]" print top

let test_abs () =
  assert_equal_t ~msg:"abs -13" "[-13, -13]" (abs (-13)) ;
  assert_equal_t ~msg:"abs 0"   "[0, 0]" (abs 0) ;
  assert_equal_t ~msg:"abs 7"   "[7, 7]" (abs 7)

let test_empty () =
  assert_bool ~msg:"empty bot"       true  (empty bot) ;
  assert_bool ~msg:"empty m3"        false (empty m3) ;
  assert_bool ~msg:"empty zero"      false (empty zero) ;
  assert_bool ~msg:"empty p5_to_inf" false (empty p5_to_inf) ;
  assert_bool ~msg:"empty top"       false (empty top)

let test_add () =
  let aux a b expected =
    assert_equal_t ~msg:(message "add" a b) expected (Op.add a b)
  in
  List.iter (fun a -> aux bot a "⊥") elements ;
  aux m3 m3 "[-6, -6]" ;
  aux m3 p5 "[2, 2]" ;
  aux p5 m3 "[2, 2]" ;
  aux zero p5 "[5, 5]" ;
  aux zero zero "[0, 0]" ;
  aux zero m3_to_inf "[-3, +∞]" ;
  aux zero top "[-∞, +∞]" ;
  aux m3_to_p5 p5 "[2, 10]" ;
  aux m3_to_p5 m3_to_p5 "[-6, 10]" ;
  aux inf_to_m3 p5 "[-∞, 2]" ;
  aux inf_to_m3 p5_to_inf "[-∞, +∞]" ;
  aux inf_to_m3 inf_to_p5 "[-∞, 2]" ;
  aux p5_to_inf m3 "[2, +∞]" ;
  aux top p5 "[-∞, +∞]" ;
  aux top top "[-∞, +∞]"

let test_sub () =
  let aux a b expected =
    assert_equal_t ~msg:(message "sub" a b) expected (Op.sub a b)
  in
  List.iter (fun a -> aux bot a "⊥") elements ;
  aux m3 m3 "[0, 0]" ;
  aux m3 p5 "[-8, -8]" ;
  aux p5 m3 "[8, 8]" ;
  aux zero p5 "[-5, -5]" ;
  aux zero zero "[0, 0]" ;
  aux zero m3_to_inf "[-∞, 3]" ;
  aux zero top "[-∞, +∞]" ;
  aux m3_to_p5 p5 "[-8, 0]" ;
  aux m3_to_p5 m3_to_p5 "[-8, 8]" ;
  aux inf_to_m3 p5 "[-∞, -8]" ;
  aux inf_to_m3 p5_to_inf "[-∞, -8]" ;
  aux inf_to_m3 inf_to_p5 "[-∞, +∞]" ;
  aux p5_to_inf m3 "[8, +∞]" ;
  aux top p5 "[-∞, +∞]" ;
  aux top top "[-∞, +∞]"

(* Additional elements to test multiplication and division. *)
let m7_to_m4 = lub (abs (-7)) (abs (-4))
let p2_to_p6 = lub (abs 2) (abs 6)

let test_mul () =
  let aux a b expected =
    assert_equal_t ~msg:(message "mul" a b) expected (Op.mul a b)
  in
  List.iter (fun a -> aux bot a "⊥") elements ;
  aux m3 m3 "[9, 9]" ;
  aux m3 p5 "[-15, -15]" ;
  aux p5 m3 "[-15, -15]" ;
  aux zero p5 "[0, 0]" ;
  aux zero zero "[0, 0]" ;
  aux zero m3_to_inf "[0, 0]" ;
  aux zero top "[0, 0]" ;
  aux m3_to_p5 p5 "[-15, 25]" ;
  aux m3_to_p5 m3_to_p5 "[-15, 25]" ;
  aux m7_to_m4 m7_to_m4 "[16, 49]" ;
  aux m7_to_m4 m3_to_p5 "[-35, 21]" ;
  aux m7_to_m4 p2_to_p6 "[-42, -8]" ;
  aux p2_to_p6 m3_to_p5 "[-18, 30]" ;
  aux p2_to_p6 p2_to_p6 "[4, 36]" ;
  aux inf_to_m3 p5 "[-∞, -15]" ;
  aux inf_to_m3 p5_to_inf "[-∞, -15]" ;
  aux inf_to_m3 inf_to_p5 "[-∞, +∞]" ;
  aux p5_to_inf m3 "[-∞, -15]" ;
  aux top p5 "[-∞, +∞]" ;
  aux top top "[-∞, +∞]"

let test_div () =
  let aux a b expected =
    assert_equal_t ~msg:(message "div" a b) expected (Op.div a b)
  in
  List.iter (fun a -> aux bot a "⊥") elements ;
  aux m3 m3 "[1, 1]" ;
  aux m3 p5 "[0, 0]" ;
  aux p5 m3 "[-1, -1]" ;
  aux zero p5 "[0, 0]" ;
  aux zero zero "⊥" ;
  aux zero m3_to_inf "[0, 0]" ;
  aux zero top "[0, 0]" ;
  aux m3_to_p5 p5 "[0, 1]" ;
  aux m3_to_p5 m3_to_p5 "[-5, 5]" ;
  aux m7_to_m4 m7_to_m4 "[0, 1]" ;
  aux m7_to_m4 m3_to_p5 "[-7, 7]" ;
  aux m7_to_m4 p2_to_p6 "[-3, 0]" ;
  aux p2_to_p6 m3_to_p5 "[-6, 6]" ;
  aux p2_to_p6 p2_to_p6 "[0, 3]" ;
  aux inf_to_m3 p5 "[-∞, 0]" ;
  aux inf_to_m3 p5_to_inf "[-∞, 0]" ;
  aux inf_to_m3 inf_to_p5 "[-∞, +∞]" ;
  aux p5_to_inf m3 "[-∞, -1]" ;
  aux top p5 "[-∞, +∞]" ;
  aux top top "[-∞, +∞]"

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
  List.iter (fun a -> if not (empty a) then aux a top top "[-∞, +∞]") elements ;
  List.iter (fun a -> aux' top top a a) elements ;
  aux m3 m3 top "[-1, -1]" ;
  aux m3 zero top "[0, 0]" ;
  aux m3 p5 top "⊥" ;
  aux m3 m7_to_m4 top "[-2, -2]" ;
  aux m3 m3_to_p5 top "[-1, 1]" ;
  aux m3 p2_to_p6 top "[1, 2]" ;
  aux m3 inf_to_p5 top "[-∞, 1]" ;
  aux m3 p5_to_inf top "[2, +∞]" ;
  aux zero m7_to_m4 top "⊥" ;
  aux zero zero top "[-∞, +∞]" ;
  aux zero p2_to_p6 top "⊥" ;
  aux p5 m3 top "⊥" ;
  aux p5 zero top "[0, 0]" ;
  aux p5 p5 top "[-1, -1]" ;
  aux p5 m7_to_m4 top "[1, 1]" ;
  aux p5 m3_to_p5 top "[-1, 0]" ;
  aux p5 p2_to_p6 top "[-1, -1]" ;
  aux p5 inf_to_m3 top "[1, +∞]" ;
  aux p5 m3_to_inf top "[-∞, 0]" ;
  aux m7_to_m4 m3 top "⊥" ;
  aux m7_to_m4 zero top "[0, 0]" ;
  aux m7_to_m4 p5 top "[1, 1]" ;
  aux m7_to_m4 m7_to_m4 top "[-1, -1]" ;
  aux m7_to_m4 m3_to_p5 top "[0, 1]" ;
  aux m7_to_m4 p2_to_p6 top "[1, 1]" ;
  aux m7_to_m4 inf_to_p5 top "[-∞, 1]" ;
  aux m7_to_m4 p5_to_inf top "[1, +∞]" ;
  aux p2_to_p6 m3 top "[1, 1]" ;
  aux p2_to_p6 zero top "[0, 0]" ;
  aux p2_to_p6 p5 top "[-2, -1]" ; (* Ideally [-1, -1] *)
  aux p2_to_p6 m7_to_m4 top "[1, 3]" ;
  aux p2_to_p6 m3_to_p5 top "[-2, 1]" ;
  aux p2_to_p6 p2_to_p6 top "[-3, -1]" ;
  aux p2_to_p6 inf_to_m3 top "[1, +∞]" ;
  aux p2_to_p6 m3_to_inf top "[-∞, 1]" ;
  aux m3_to_p5 m3 top "[-3, 3]" ;
  aux m3_to_p5 zero top "[-∞, +∞]" ;
  aux m3_to_p5 p5 top "[-5, 5]" ;
  aux m3_to_p5 m7_to_m4 top "[-7, 7]" ;
  aux m3_to_p5 m3_to_p5 top "[-∞, +∞]" ;
  aux m3_to_p5 p2_to_p6 top "[-6, 6]" ;
  aux m3_to_p5 inf_to_m3 top "[-∞, +∞]" ;
  aux m3_to_p5 p5_to_inf top "[-∞, +∞]" ;
  aux inf_to_m3 m3 top "[-1, -1]" ;
  aux inf_to_m3 zero top "[0, 0]" ;
  aux inf_to_m3 p5 top "[1, 1]" ;
  aux inf_to_m3 m7_to_m4 top "[-2, -1]" ;
  aux inf_to_m3 m3_to_p5 top "[-1, 1]" ;
  aux inf_to_m3 p2_to_p6 top "[1, 2]" ;
  aux inf_to_m3 inf_to_p5 top "[-∞, 1]" ;
  aux inf_to_m3 p5_to_inf top "[1, +∞]" ;
  aux p5_to_inf m3 top "⊥" ;
  aux p5_to_inf zero top "[0, 0]" ;
  aux p5_to_inf p5 top "[-1, -1]" ;
  aux p5_to_inf m7_to_m4 top "[1, 1]" ;
  aux p5_to_inf m3_to_p5 top "[-1, 0]" ;
  aux p5_to_inf p2_to_p6 top "[-1, -1]" ;
  aux p5_to_inf inf_to_m3 top "[1, +∞]" ;
  aux p5_to_inf m3_to_inf top "[-∞, 0]" ;
  aux top m3 top "[-3, 3]" ;
  aux top zero top "[-∞, +∞]" ;
  aux top p5 top "[-5, 5]" ;
  aux top m7_to_m4 top "[-7, 7]" ;
  aux top m3_to_p5 top "[-∞, +∞]" ;
  aux top p2_to_p6 top "[-6, 6]" ;
  aux top inf_to_m3 top "[-∞, +∞]" ;
  aux top p5_to_inf top "[-∞, +∞]" ;
  (*
   * Examples where (equality a b c) is more precise than (equality a b top)
   * intersected with c.
   *)
  aux m3_to_p5 m7_to_m4 zero "⊥" ;
  aux m3_to_p5 p2_to_p6 zero "⊥"

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
  List.iter (fun a -> if not (empty a) then aux a top top "[-∞, +∞]") elements ;
  List.iter (fun a -> aux' top top a a) elements ;
  aux m3 p5 top "[2, +∞]" ;
  aux m3 inf_to_p5 top "[-∞, +∞]" ;
  aux zero m7_to_m4 top "[-∞, +∞]" ;
  aux zero p2_to_p6 top "⊥" ;
  aux p5 m3 top "[-∞, 0]" ;
  aux p5 m3_to_inf top "[-∞, 0]" ;
  aux m7_to_m4 m3_to_p5 top "[0, +∞]" ;
  aux p2_to_p6 m3_to_p5 top "[-∞, 1]" ;
  aux m3_to_p5 p5 top "[-∞, +∞]" ;
  aux inf_to_m3 m7_to_m4 top "[-2, +∞]" ;
  aux inf_to_m3 m3_to_p5 top "[-1, +∞]" ;
  aux inf_to_m3 p2_to_p6 top "[1, +∞]" ;
  aux p5_to_inf m7_to_m4 top "[-∞, 1]" ;
  aux p5_to_inf m3_to_p5 top "[-∞, 0]" ;
  aux p5_to_inf p2_to_p6 top "[-∞, -1]" ;
  aux top m3 top "[-∞, +∞]" ;
  aux top p5 top "[-∞, +∞]"

let test_widen () =
  let aux a b expected =
    assert_equal_t ~msg:(message "widen" a b) expected (widen a b)
  in
  List.iter (fun a -> aux top a "[-∞, +∞]") elements ;
  List.iter (fun a -> aux a top "[-∞, +∞]") elements ;
  aux bot zero "[0, 0]" ;
  aux bot m3_to_p5 "[-3, 5]" ;
  aux bot inf_to_p5 "[-∞, 5]" ;
  aux bot m3_to_inf "[-3, +∞]" ;
  aux zero bot "[0, 0]" ;
  aux zero m3 "[-∞, 0]" ;
  aux zero zero "[0, 0]" ;
  aux zero p5 "[0, +∞]" ;
  aux zero m3_to_p5 "[-∞, +∞]" ;
  aux zero inf_to_m3 "[-∞, 0]" ;
  aux zero inf_to_p5 "[-∞, +∞]" ;
  aux zero m3_to_inf "[-∞, +∞]" ;
  aux zero p5_to_inf "[0, +∞]" ;
  aux m3_to_p5 bot "[-3, 5]" ;
  aux m3_to_p5 zero "[-3, 5]" ;
  aux m3_to_p5 inf_to_m3 "[-∞, 5]" ;
  aux m3_to_p5 inf_to_p5 "[-∞, 5]" ;
  aux m3_to_p5 m3_to_inf "[-3, +∞]" ;
  aux m3_to_p5 p5_to_inf "[-3, +∞]" ;
  aux inf_to_m3 bot "[-∞, -3]" ;
  aux inf_to_m3 m3 "[-∞, -3]" ;
  aux inf_to_m3 zero "[-∞, +∞]" ;
  aux inf_to_m3 m3_to_p5 "[-∞, +∞]" ;
  aux inf_to_m3 inf_to_m3 "[-∞, -3]" ;
  aux inf_to_m3 inf_to_p5 "[-∞, +∞]" ;
  aux inf_to_m3 m3_to_inf "[-∞, +∞]" ;
  aux inf_to_m3 p5_to_inf "[-∞, +∞]" ;
  aux p5_to_inf bot "[5, +∞]" ;
  aux p5_to_inf p5 "[5, +∞]" ;
  aux p5_to_inf zero "[-∞, +∞]" ;
  aux p5_to_inf m3_to_p5 "[-∞, +∞]" ;
  aux p5_to_inf inf_to_m3 "[-∞, +∞]" ;
  aux p5_to_inf inf_to_p5 "[-∞, +∞]" ;
  aux p5_to_inf m3_to_inf "[-∞, +∞]" ;
  aux p5_to_inf p5_to_inf "[5, +∞]"

let test_narrow () =
  let aux a b expected =
    assert_equal_t ~msg:(message "narrow" a b) expected (narrow a b)
  in
  List.iter (fun a -> aux a bot "⊥") elements ;
  aux zero zero "[0, 0]" ;
  aux m3_to_p5 zero "[-3, 5]" ;
  aux inf_to_m3 m3 "[-3, -3]" ;
  aux inf_to_m3 inf_to_m3 "[-∞, -3]" ;
  aux inf_to_p5 m3 "[-3, 5]" ;
  aux inf_to_p5 zero "[0, 5]" ;
  aux inf_to_p5 m3_to_p5 "[-3, 5]" ;
  aux inf_to_p5 inf_to_m3 "[-∞, 5]" ;
  aux inf_to_p5 inf_to_p5 "[-∞, 5]" ;
  aux m3_to_inf zero "[-3, 0]" ;
  aux m3_to_inf p5 "[-3, 5]" ;
  aux m3_to_inf m3_to_p5 "[-3, 5]" ;
  aux m3_to_inf m3_to_inf "[-3, +∞]" ;
  aux m3_to_inf p5_to_inf "[-3, +∞]" ;
  aux p5_to_inf p5 "[5, 5]" ;
  aux p5_to_inf p5_to_inf "[5, +∞]" ;
  aux top zero "[0, 0]" ;
  aux top m3_to_p5 "[-3, 5]" ;
  aux top inf_to_p5 "[-∞, 5]" ;
  aux top m3_to_inf "[-3, +∞]" ;
  aux top top "[-∞, +∞]"

(* Create the generic lattice test cases. *)
module M =
struct
  module L = DomInterval
  let elements = elements
  let relation =
    [
      (bot, m3) ;
      (bot, zero) ;
      (bot, p5) ;
      (m3, inf_to_m3) ;
      (m3, m3_to_p5) ;
      (zero, m3_to_p5) ;
      (p5, m3_to_p5) ;
      (p5, p5_to_inf) ;
      (m3_to_p5, inf_to_p5) ;
      (m3_to_p5, m3_to_inf) ;
      (inf_to_m3, inf_to_p5) ;
      (p5_to_inf, m3_to_inf) ;
      (inf_to_p5, top) ;
      (m3_to_inf, top) ;
    ]
end

module T = TestLattice.Make (M)

(* Collection of all tests. *)
let alltests =
  [
    "dominterval.print", test_print ;
  ]
  @
  (T.tests "dominterval")
  @
  [
    "dominterval.abs", test_abs ;
    "dominterval.empty", test_empty ;
    "dominterval.add", test_add ;
    "dominterval.sub", test_sub ;
    "dominterval.mul", test_mul ;
    "dominterval.div", test_div ;
    "dominterval.equality", test_equality ;
    "dominterval.inequality", test_inequality ;
    "dominterval.widen", test_widen ;
    "dominterval.narrow", test_narrow ;
  ]

(* This test suite. *)
let suite = ("DomInterval", alltests)
