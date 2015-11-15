(* $Id: Print.ml 3251 2015-10-29 15:56:15Z sutre $ *)


(*
 * Signatures and helper functions for pretty-printing.
 *)


module type S =
sig
  type t
  val print : Format.formatter -> t -> unit
end


let list_printer_from_printer sep printer fmt list =
  match list with
  | []  -> ()
  | head::tail ->
     Format.fprintf fmt "@[%a@]" printer head ;
     List.iter
       (fun data ->
        Format.fprintf fmt sep ;
        Format.fprintf fmt "@[%a@]" printer data)
       tail

let array_printer_from_printer sep printer fmt array =
  if (Array.length array) > 0 then
    let n = Array.length array
    in
    for i = 0 to n - 2 do
      Format.fprintf fmt "@[%a@]" printer (i, array.(i)) ;
      Format.fprintf fmt sep
    done ;
    Format.fprintf fmt "@[%a@]" printer (n-1, array.(n-1))
