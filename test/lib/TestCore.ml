(* $Id: TestCore.ml 3275 2015-11-01 21:50:37Z sutre $ *)


(*
 * Minimalistic unit testing library.
 *)


(*
 * Configuration
 *)

let verbosity = ref 1

(* Helper function to log messages or ignore them, depending on their level. *)
let log lvl =
  if lvl <= !verbosity then
    Format.fprintf Format.std_formatter
  else
    Format.ifprintf Format.std_formatter

(*
 * Main functions
 *)

exception Test_failure of string

let failure msg = raise (Test_failure msg)

(* Messages to indicate the result of a test case. *)
let pass_msg = "PASS"
and fail_msg = "FAIL"

(* Max display width of a test name (used when !verbosity <= 1). *)
let name_width = 24

let run (name, tests) =
  let pass_count = ref 0
  and fail_count = ref 0
  in
  let run_test (name, test) =
    let name =
      if !verbosity > 1 then
        name
      else
        if String.length name > name_width then
          (* Truncate name if it is too long. *)
          (String.sub name 0 (name_width - 1)) ^ ".    "
        else
          (* Append spaces otherwise. *)
          name ^ (String.make (name_width - (String.length name) + 4) ' ')
    in
    log 1 "@," ;
    log 2 "@[<v 4>" ;
    log 1 "@[<h>%s@]" name ;
    begin
      try
        test () ;
        log 2 "@]@," ;
        log 1 "@[<h>%s@]" pass_msg ;
        incr pass_count
      with
        Test_failure msg ->
        log 2 "@]@," ;
        log 1 "@[<h>%s@ (%s)@]" fail_msg msg ;
        incr fail_count
    end
  in
  log 0 "@[<v 4>@[Test@ suite:@ %s@]" name ;
  List.iter run_test tests ;
  log 0 "@]@." ;
  log 0 "@[Passed:@ %3d@]@." !pass_count ;
  let (alert_begin, alert_end) =
    if !fail_count > 0 then
      ("\x1B[1;31m", "\x1B[0m")
    else
      ("", "")
  in
  log 0 "@[%sFailed:@ %3d%s@]@." alert_begin !fail_count alert_end ;
  !fail_count = 0

(*
 * Assertions.
 *)

let assert_equal ~equal ~print ?(msg = "") expected actual =
  log 2
      "@,@[<h>[assert_equal]@ \"%s\"@ exp@ %a@ act@ %a@]"
      msg print expected print actual ;
  let sep = if msg = "" then format_of_string "" else format_of_string ":@ "
  in
  if not (equal expected actual) then
    failure
      (Format.asprintf
         ("@[<h>%s" ^^ sep ^^ "expected@ `%a'@ but@ got@ `%a'@]")
         msg print expected print actual)

let assert_bool = assert_equal ~equal:(=) ~print:Format.pp_print_bool
and assert_int = assert_equal ~equal:(=) ~print:Format.pp_print_int
and assert_string = assert_equal ~equal:(=) ~print:Format.pp_print_string

let assert_print ?(msg = "") expected print actual =
  assert_string ~msg expected (Format.asprintf "@[%a@]" print actual)
