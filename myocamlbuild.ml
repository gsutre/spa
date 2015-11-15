(* $Id: myocamlbuild.ml 3379 2015-11-15 16:05:22Z sutre $ *)

open Ocamlbuild_plugin

;;

dispatch
  begin
    function
    | After_rules ->
       begin
         flag ["ocaml" ; "compile" ; "unsafe"] (A "-unsafe") ;
         flag ["ocaml" ; "compile" ; "noassert"] (A "-noassert") ;
         flag ["ocaml" ; "compile" ; "warn_-4"] (S [A "-w" ; A "-4"]) ;
         flag ["ocaml" ; "compile" ; "warn_-44"] (S [A "-w" ; A "-44"])
       end
    | _ -> ()
  end
