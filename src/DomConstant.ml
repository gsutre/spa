(* $Id: DomConstant.ml 3366 2015-11-13 09:22:07Z sutre $ *)


(*
 * Numerical domain for constant propagation analysis.
 *)


type t =
  | Bot
  | Val of int
  | Top

let print fmt =
  function
  | Bot -> Format.pp_print_string fmt "⊥"
  | Val x -> Format.pp_print_int fmt x
  | Top -> Format.pp_print_string fmt "⊤"

let bot = Bot
let top = Top

let equal = (=)

let leq a b =
  match (a, b) with
  | (Bot, _)
  | (_, Top) -> true
  | (_, Bot)
  | (Top, _) -> false
  | (Val x, Val y) -> x = y

let glb a b =
  match (a, b) with
  | (Bot, _)
  | (_, Bot) -> Bot
  | (Top, c)
  | (c, Top) -> c
  | (Val x, Val y) -> if x = y then a else Bot

let lub a b =
  match (a, b) with
  | (Top, _)
  | (_, Top) -> Top
  | (Bot, c)
  | (c, Bot) -> c
  | (Val x, Val y) -> if x = y then a else Top

let abs x = Val x

let empty a = equal a bot

module Op =
struct
  let add a b =
    match (a, b) with
    | (Bot, _)
    | (_, Bot) -> Bot
    | (Top, _)
    | (_, Top) -> Top
    | (Val x, Val y) -> Val (x+y)

  let sub a b =
    match (a, b) with
    | (Bot, _)
    | (_, Bot) -> Bot
    | (Top, _)
    | (_, Top) -> Top
    | (Val x, Val y) -> Val (x-y)

  let mul a b =
    match (a, b) with
    | (Bot, _)
    | (_, Bot) -> Bot
    | (Val 0, _)
    | (_, Val 0) -> Val 0
    | (Top, _)
    | (_, Top) -> Top
    | (Val x, Val y) -> Val (x*y)

  let div a b =
    match (a, b) with
    | (Bot, _)
    | (_, Bot)
    | (_, Val 0) -> Bot
    | (Val 0, _) -> Val 0
    | (Top, _)
    | (_, Top) -> Top
    | (Val x, Val y) -> Val (x/y)

  let equality a b c =
    match (a, b, c) with
    | (Bot, _, _)
    | (_, Bot, _)
    | (_, _, Bot) -> Bot
    | (_, Top, _)
    | (Top, Val _, Top) -> c
    | (Val 0, Val s, Top)
    | (Top, Val s, Val 0) -> if s = 0 then c else Bot
    | (Top, Val s, Val x) -> if s mod x = 0 then c else Bot
    | (Val r, Val s, Top) -> if s mod r = 0 then Val (-s/r) else Bot
    | (Val r, Val s, Val x) -> if (r * x) + s = 0 then c else Bot

  let inequality a b c =
    match (a, b, c) with
    | (Bot, _, _)
    | (_, Bot, _)
    | (_, _, Bot) -> Bot
    | (_, Top, _)
    | (Top, Val _, Top) -> c
    | (Val 0, Val s, Top)
    | (Top, Val s, Val 0) -> if s <= 0 then c else Bot
    | (Top, Val _, Val _)
    | (Val _, Val _, Top) -> c
    | (Val r, Val s, Val x) -> if (r * x) + s <= 0 then c else Bot
end

let widen = lub

let narrow a b =
  assert (leq b a) ;
  b
