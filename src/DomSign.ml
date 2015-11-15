(* $Id: DomSign.ml 3379 2015-11-15 16:05:22Z sutre $ *)


(*
 * Numerical domain for sign analysis.
 *)


(*
 * An abstract element is a triple of booleans (n, z, p) where n stands for
 * negative, z for zero and p for positive.  The concretization of (n, z, p) is
 * the set of integers x satisfying the following formula:
 *
 *                (n ∧ x < 0)  ∨  (z ∧ x = 0)  ∨  (p ∧ x > 0)
 *
 * Examples:         Abstract triple            Concretization
 *                (true , false, true )         { x | x ≠ 0 }
 *                (false, true , false)         { 0 }
 *                (false, true , true )         { x | x ≥ 0 }
 *                (true , false, false)         { x | x < 0 }
 *
 * Put differently, the sign abstract domain is induced by the partition of the
 * integers into negative, zero and positive integers.  Triples (n, z, p) encode
 * unions of classes in the obvious way.
 *)
type t = bool * bool * bool

let print fmt =
  function
  | (false, false, false) -> Format.pp_print_string fmt "⊥"
  | (true , false, false) -> Format.pp_print_string fmt "-"
  | (false, true , false) -> Format.pp_print_string fmt "0"
  | (false, false, true ) -> Format.pp_print_string fmt "+"
  | (true , true , false) -> Format.pp_print_string fmt "-0"
  | (true , false, true ) -> Format.pp_print_string fmt "-+"
  | (false, true , true ) -> Format.pp_print_string fmt "0+"
  | (true , true , true ) -> Format.pp_print_string fmt "⊤"

let bot = (false, false, false)
let top = (true , true , true )

let equal = (=)

let leq (n1, z1, p1) (n2, z2, p2) = (n1 <= n2) && (z1 <= z2) && (p1 <= p2)

let glb (n1, z1, p1) (n2, z2, p2) = (min n1 n2, min z1 z2, min p1 p2)

let lub (n1, z1, p1) (n2, z2, p2) = (max n1 n2, max z1 z2, max p1 p2)

let abs x = (x < 0, x = 0, x > 0)

let empty a = equal a bot

module Op =
struct
  let neg (n, _, _) = n
  let zer (_, z, _) = z
  let pos (_, _, p) = p

  let add a b =
    if equal a bot || equal b bot then bot
    else
      let n = neg a || neg b
      and z = (neg a && pos b) || (zer a && zer b) || (pos a && neg b)
      and p = pos a || pos b
      in
      (n, z, p)

  let sub a b =
    if equal a bot || equal b bot then bot
    else
      let n = neg a || pos b
      and z = (neg a && neg b) || (zer a && zer b) || (pos a && pos b)
      and p = pos a || neg b
      in
      (n, z, p)

  let mul a b =
    if equal a bot || equal b bot then bot
    else
      let n = (neg a && pos b) || (pos a && neg b)
      and z = zer a || zer b
      and p = (neg a && neg b) || (pos a && pos b)
      in
      (n, z, p)

  let div a b =
    if equal a bot || equal b bot then bot
    else
      let n = (neg a && pos b) || (pos a && neg b)
      and z = neg b || pos b
      and p = (neg a && neg b) || (pos a && pos b)
      in
      (n, z, p)

  let equality a b c =
    let n =
      neg c &&
        ((neg a && neg b) || (zer a && zer b) || (pos a && pos b))
    and z =
      zer c &&
        (neg a || zer a || pos a) && zer b
    and p =
      pos c &&
        ((neg a && pos b) || (zer a && zer b) || (pos a && neg b))
    in
    (n, z, p)

  let inequality a b c =
    let b' = add b (false, true, true)
    in
    equality a b' c
end

let widen = lub

let narrow a b =
  assert (leq b a) ;
  b
