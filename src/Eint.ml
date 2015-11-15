(* $Id: Eint.ml 3320 2015-11-08 21:43:15Z sutre $ *)


(*
 * Extended integers.
 *)


type eint =
  | Integer of int
  | Neg_infty
  | Pos_infty

let pp_print_eint fmt =
  function
  | Neg_infty -> Format.pp_print_string fmt "-∞"
  | Pos_infty -> Format.pp_print_string fmt "+∞"
  | Integer x -> Format.pp_print_int fmt x

(* Integer division with rounding (helper function for ( / ) below). *)
let division ?(rounding = Integer 0) (x : int) (y : int) =
  let d = x / y
  in
  if x mod y = 0 then
    d
  else
    (* Both x and y are non-zero. *)
    if (x > 0 && y > 0) || (x < 0 && y < 0) then
      (* The real division of x by y is greater than d. *)
      match rounding with
      | Neg_infty -> d
      | Integer r -> if r <= d then d else d + 1
      | Pos_infty -> d + 1
    else
      (* The real division of x by y is lesser than d. *)
      match rounding with
      | Pos_infty -> d
      | Integer r -> if r >= d then d else d - 1
      | Neg_infty -> d - 1

(* Division by +∞ with rounding (helper function for ( / ) below). *)
let division_by_pos_infty ?(rounding = Integer 0) (x : int) =
  if x = 0 then 0
  else if x > 0 then
    (* The real division of x by +∞ corresponds to a small ε > 0. *)
    match rounding with
    | Neg_infty -> 0
    | Integer r -> if r <= 0 then 0 else 1
    | Pos_infty -> 1
  else
    (* The real division of x by +∞ corresponds to a small ε < 0. *)
    match rounding with
    | Pos_infty -> 0
    | Integer r -> if r >= 0 then 0 else -1
    | Neg_infty -> -1

(* Division by -∞ with rounding (helper function for ( / ) below). *)
let division_by_neg_infty ?(rounding = Integer 0) (x : int) =
  division_by_pos_infty ~rounding (- x)

let ( = ) = (=)

let ( < ) x y =
  match (x, y) with
  | (_, Neg_infty) -> false
  | (Neg_infty, _) -> true
  | (Pos_infty, _) -> false
  | (_, Pos_infty) -> true
  | (Integer x, Integer y) -> x < y

let ( <= ) x y =
  match (x, y) with
  | (Neg_infty, _) -> true
  | (_, Neg_infty) -> false
  | (_, Pos_infty) -> true
  | (Pos_infty, _) -> false
  | (Integer x, Integer y) -> x <= y

let ( > ) x y = (<) y x
let ( >= ) x y = (<=) y x

let min x y =
  match (x, y) with
  | (Neg_infty, _)
  | (_, Neg_infty) -> Neg_infty
  | (Pos_infty, z)
  | (z, Pos_infty) -> z
  | (Integer x, Integer y) -> Integer (min x y)

let max x y =
  match (x, y) with
  | (Pos_infty, _)
  | (_, Pos_infty) -> Pos_infty
  | (Neg_infty, z)
  | (z, Neg_infty) -> z
  | (Integer x, Integer y) -> Integer (max x y)

let ( + ) x y =
  match (x, y) with
  | (Neg_infty, Pos_infty)
  | (Pos_infty, Neg_infty) -> invalid_arg "Eint.(+)"
  | (Neg_infty, _)
  | (_, Neg_infty) -> Neg_infty
  | (Pos_infty, _)
  | (_, Pos_infty) -> Pos_infty
  | (Integer x, Integer y) -> Integer (x + y)

let ( - ) x y =
  match (x, y) with
  | (Neg_infty, Neg_infty)
  | (Pos_infty, Pos_infty) -> invalid_arg "Eint.(-)"
  | (Neg_infty, _)
  | (_, Pos_infty) -> Neg_infty
  | (Pos_infty, _)
  | (_, Neg_infty) -> Pos_infty
  | (Integer x, Integer y) -> Integer (x - y)

let ( * ) x y =
  match (x, y) with
  | (Integer 0, _)
  | (_, Integer 0) -> Integer 0
  | (Neg_infty, z)
  | (z, Neg_infty) -> if z < Integer 0 then Pos_infty else Neg_infty
  | (Pos_infty, z)
  | (z, Pos_infty) -> if z < Integer 0 then Neg_infty else Pos_infty
  | (Integer x, Integer y) -> Integer (x * y)

let ( / ) ?(rounding = Integer 0) x y =
  match (x, y) with
  | (Integer x, Neg_infty) -> Integer (division_by_neg_infty ~rounding x)
  | (Integer x, Pos_infty) -> Integer (division_by_pos_infty ~rounding x)
  | (_, Neg_infty)
  | (_, Pos_infty) -> invalid_arg "Eint.(/)"
  | (_, Integer 0) -> raise Division_by_zero
  | (Neg_infty, z) -> if z < Integer 0 then Pos_infty else Neg_infty
  | (Pos_infty, z) -> if z < Integer 0 then Neg_infty else Pos_infty
  | (Integer x, Integer y) -> Integer (division ~rounding x y)

let ( ~+ ) x = x
let ( ~- ) x = (Integer 0) - x
