(* $Id: Print.mli 3251 2015-10-29 15:56:15Z sutre $ *)


(**
 * Signature and helper functions for pretty-printing.
 *
 * This module follows the standard OCaml pretty-printing facility provided in
 * the module [Format] of the standard library.  In particular, pretty-printing
 * commands assume that there is an opened pretty-printing box.  This permits
 * more flexibility, since the choice of the enclosing pretty-printing box may
 * depend on the context.
 *
 * @see <http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html> Format
 *)


(**
 * Signature for a type equipped with a pretty-printing function.
 *)
module type S =
sig
  (**
   * The type.
   *)
  type t

  (**
   * A pretty-printer for this type.  This function prints values of type [t] in
   * the current pretty-printing box.
   *)
  val print : Format.formatter -> t -> unit
end


(**
 * [list_printer_from_printer sep printer] returns a pretty-printer for lists of
 * type ['a list], given a separator format [sep] and a pretty-printer [printer]
 * for values of type ['a].  The resulting pretty-printer prints each element of
 * its argument list and prints [sep] between each element.  Each element in the
 * list is printed in a new enclosing pretty-printing box.
 * In other words, [list_printer_from_printer sep printer fmt [a1; ...; aN]] is
 * equivalent to
 {[
   begin
     Format.fprintf fmt "@[%a@]" printer a1 ;
     Format.fprintf fmt sep ;
     ...
     Format.fprintf fmt sep ;
     Format.fprintf fmt "@[%a@]" printer aN
   end
 ]}
 * Note that the separator [sep] may contain pretty-printing commands.  For
 * instance [";@ "] could be used as a separator argument to this function.
 *)
val list_printer_from_printer :
  (unit, Format.formatter, unit) format ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a list -> unit

(**
 * [array_printer_from_printer sep printer] returns a pretty-printer for arrays
 * of type ['a array], given a separator format [sep] and a pretty-printer
 * [printer] for values of type [int * 'a].  The resulting pretty-printer prints
 * each pair [(i, a.(i))] of its argument array [a] and prints [sep] between
 * each pair.  Each pair in the array is printed in a new enclosing
 * pretty-printing box.
 * In other words, [array_printer_from_printer sep printer fmt [|a1; ...; aN|]]
 * is equivalent to
 {[
   begin
     Format.fprintf fmt "@[%a@]" printer (0, a1) ;
     Format.fprintf fmt sep ;
     ...
     Format.fprintf fmt sep ;
     Format.fprintf fmt "@[%a@]" printer (N-1, aN)
   end
 ]}
 * Note that the separator [sep] may contain pretty-printing commands.  For
 * instance [";@ "] could be used as a separator argument to this function.
 *)
val array_printer_from_printer :
  (unit, Format.formatter, unit) format ->
  (Format.formatter -> (int * 'a) -> unit) ->
  Format.formatter -> 'a array -> unit
