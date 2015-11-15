(* $Id: AutLexer.mll 3251 2015-10-29 15:56:15Z sutre $ *)

{
  open AutParser
  exception Error
}

let digit = ['0'-'9']
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9' '\'' '_']

rule token = parse
  (* Ignore spaces, tabs and carriage returns. *)
  | [' ' '\t' '\r']     { token lexbuf }
  (* Ignore comments. *)
  | '#' ([^'\n']*)      { token lexbuf }
  (* Update line count on new lines. *)
  | '\n'                { Lexing.new_line lexbuf ; token lexbuf }

  (* Keywords. *)
  | "var"               { TK_VAR }
  | "from"              { TK_FROM }
  | "initial"           { TK_INITIAL }
  | "final"             { TK_FINAL }
  | "|"                 { TK_CHOICE }
  | "-->"               { TK_ARROW }
  | ";"                 { TK_SEMICOLON }
  | ","                 { TK_COMMA }
  | ":="                { TK_ASSIGN }
  | "skip"              { TK_SKIP }
  | "+"                 { TK_ADD }
  | "-"                 { TK_SUB }
  | "*"                 { TK_MUL }
  | "/"                 { TK_DIV }
  | "=="                { TK_EQ }
  | "<"                 { TK_LST }
  | ">"                 { TK_GST }
  | "<="                { TK_LEQ }
  | ">="                { TK_GEQ }
  | '('                 { TK_LPAREN }
  | ')'                 { TK_RPAREN }
  | '{'                 { TK_LBRACE }
  | '}'                 { TK_RBRACE }

  (* Natural numbers. *)
  | (digit)+ as s       { TK_INT(int_of_string s) }

  (* End of file. *)
  | eof                 { TK_EOF }

  (* Strings. *)
  | (alphanum)+ as s    { TK_STR(s) }

  (* Fail if nothing else matched. *)
  | _                   { raise Error }
