(* $Id: spa.ml 3379 2015-11-15 16:05:22Z sutre $ *)


(*
 * Main (start of the spa program).
 *)


(* Exception for errors that are local to this module (should not happen). *)
exception Internal_error

(* Module to parse the command-line and store user-specified options. *)
module CommandLine =
struct
  (* Direction of the analysis.  Either "fwd" or "bwd". *)
  let analysis_opt = ref "fwd"

  (* Numerical domain.  Either "constant" or "sign" or "interval". *)
  let domain_opt = ref "interval"

  (* Widening delay.  Widening is disabled when negative. *)
  let widening_delay_opt = ref 0

  (* Narrowing delay.  Narrowing is disabled when negative. *)
  let narrowing_delay_opt = ref 0

  (* Verbosity switch. *)
  let verbosity_opt = ref false

  (* Specification of command-line options. *)
  let arg_spec_list = [
    ("-analysis",
     Arg.Symbol
       (["fwd" ; "bwd"], fun s -> analysis_opt := s),
     " direction of the analysis" ^
       " (default: " ^ !analysis_opt ^ ")")
    ;
    ("-domain",
     Arg.Symbol
       (["constant" ; "sign" ; "interval"], fun s -> domain_opt := s),
     " numerical domain" ^
       " (default: " ^ !domain_opt ^ ")")
    ;
    ("-widening-delay",
     Arg.Set_int widening_delay_opt,
     "<int> widening delay; a negative value disables widening" ^
       " (default: " ^ (string_of_int !widening_delay_opt) ^ ")")
    ;
    ("-narrowing-delay",
     Arg.Set_int narrowing_delay_opt,
     "<int> narrowing delay; a negative value disables narrowing" ^
       " (default: " ^ (string_of_int !narrowing_delay_opt) ^ ")")
    ;
    ("-v",
     Arg.Set verbosity_opt,
     " make the fixpoint engine verbose" ^
       " (default: " ^ (string_of_bool !verbosity_opt) ^ ")")
  ]

  let usage_msg = "Usage: " ^ (Sys.argv.(0)) ^ " [option ...] [source-file]\n"

  (* Parses the command line and returns the input file's name, if any. *)
  let parse () =
    let filename = ref None
    in
    Arg.parse
      (Arg.align arg_spec_list)
      (fun a ->
       match !filename with
       | None ->
          filename := Some a
       | Some _ ->
          raise (Arg.Bad ("unexpected argument `" ^ a ^
                            "' (multiple input files are not allowed)")))
      usage_msg ;
    !filename
end

(* Parse the command line. *)
let filename = CommandLine.parse ()

(* Parse the input file. *)
let automaton =
  let (ic, fn) =
    match filename with
    | None -> (stdin, "<stdin>")
    | Some f -> (open_in f, f)
  in
  let a =
    try
      Automaton.read ic
    with Automaton.Read_error msg ->
      Format.eprintf "@[%s:@ %s@]@." fn msg ;
      close_in ic ;
      exit 1
  in
  close_in ic ;
  a

(* Create the numerical domain. *)
let numerical_domain : (module NumericalDomain.S) =
  match !CommandLine.domain_opt with
  | "constant" -> (module DomConstant)
  | "sign"     -> (module DomSign)
  | "interval" -> (module DomInterval)
  | _ -> raise Internal_error

module N = (val numerical_domain : NumericalDomain.S)

(* Create the abstract domain and instantiate the fixpoint engine. *)
module A = PointwiseLifting.Make (N)
module F = RoundRobin.Make (Automaton) (A)

(* Account for the user-specified direction of the analysis. *)
let (automaton, transfer, transfer_name) =
  match !CommandLine.analysis_opt with
  | "fwd" -> (automaton, A.post, "post")
  | "bwd" -> (Automaton.reverse automaton, A.pre, "pre")
  | _ -> raise Internal_error

(* Print the automaton, the numerical domain and the transfer name. *)
let _ =
  Format.printf "@[%a@]@.@." Automaton.print automaton ;
  Format.printf "@[Numerical domain:  %s@]@." !CommandLine.domain_opt ;
  Format.printf "@[Transfer function: abstract %s-image@]@." transfer_name

(* Construct the monotonic dataflow problem. *)
let constant loc =
  if Automaton.Node.equal loc (Automaton.initial automaton) then
    A.top
  else
    A.bot

let problem =
  { F.graph = automaton ;
    F.transfer = transfer ;
    F.constant = constant }

(* Define the fixpoint engine's control parameters. *)
let param =
  { F.widening = A.widen ;
    F.widening_delay = !CommandLine.widening_delay_opt ;
    F.narrowing = A.narrow ;
    F.narrowing_delay = !CommandLine.narrowing_delay_opt ;
    F.verbose = !CommandLine.verbosity_opt }

(* Perform the analysis: compute a conservative solution. *)
let tbl = F.Table.create (Automaton.nodes automaton) A.bot
let () = F.solve problem param tbl

(* Display the solution. *)
let _ =
  Format.printf
    "@.@[<v 3>Conservative solution:@,%a@]@." F.Table.print tbl

(* Perform the analysis: refine the solution. *)
let () = F.refine problem param tbl

(* Display the solution. *)
let _ =
  Format.printf
    "@.@[<v 3>Refined solution:@,%a@]@." F.Table.print tbl

(* Exit. *)
let _ = exit 0
