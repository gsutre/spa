(* $Id: AutParser.mly 3379 2015-11-15 16:05:22Z sutre $ *)

%{
  let scan_declaration (vars, init, final, trans) decl =
    match decl with
    | `Variables vl -> (vars @ vl, init, final, trans)
    | `Initial loc -> (vars, Some loc, final, trans)
    | `Final loc -> (vars, init, Some loc, trans)
    | `Location (loc, out) -> (vars, init, final, (loc, out) :: trans)

  let output name decls =
    let (vars, init, final, trans) =
      List.fold_left
        scan_declaration
        ([], None, None, [])
        decls
    in
    (name, vars, init, final, trans)
%}

%token TK_VAR
%token TK_FROM
%token TK_INITIAL
%token TK_FINAL
%token TK_CHOICE
%token TK_ARROW
%token TK_SEMICOLON
%token TK_COMMA
%token TK_ASSIGN
%token TK_SKIP
%token TK_ADD
%token TK_SUB
%token TK_MUL
%token TK_DIV
%token TK_EQ
%token TK_LST
%token TK_GST
%token TK_LEQ
%token TK_GEQ
%token TK_LPAREN TK_RPAREN
%token TK_LBRACE TK_RBRACE

%token <int> TK_INT
%token <string> TK_STR
%token TK_EOF

%left TK_ADD TK_SUB     /* lowest precedence */
%left TK_MUL TK_DIV     /* medium precedence */
%nonassoc UMINUS        /* highest precedence */

(*
 * Returns a tuple (name, vars, init, final, trans) where:
 *
 * - name is the automaton's name,
 * - vars is the list of declared variables,
 * - init is the initial location (if any),
 * - final is the final location (if any),
 * - trans gives the transitions as a list of pairs (loc, out) where:
 *   . loc is a location, and
 *   . out is the list of its outbound transitions.
 *
 * The lists vars and out are in the same order as in the input file.
 *)
%start <string * string list * string option * string option *
          (string * (Command.t * string) list) list> main

%%

main:
| TK_STR TK_LBRACE list(decl) TK_RBRACE TK_EOF
  { output $1 $3 }

decl:
| variables             { `Variables $1 }
| initial               { `Initial $1 }
| final                 { `Final $1 }
| from                  { `Location $1 }

variables:
| TK_VAR TK_STR list(comma_string) TK_SEMICOLON
  { $2 :: $3 }

comma_string:
| TK_COMMA TK_STR       { $2 }

initial:
| TK_INITIAL TK_STR TK_SEMICOLON
  { $2 }

final:
| TK_FINAL TK_STR TK_SEMICOLON
  { $2 }

from:
| TK_FROM TK_STR list(transition)
  { ($2, $3) }

transition:
| TK_CHOICE command TK_ARROW TK_STR TK_SEMICOLON
  { ($2, $4) }

command:
| assign                { $1 }
| guard                 { $1 }
| skip                  { $1 }

assign:
| TK_STR TK_ASSIGN expression
  { Command.Assign ($1, $3) }

guard:
| expression relop expression
  { Command.Guard ($1, $2, $3) }

skip:
| TK_SKIP               { Command.Skip }

relop:
| TK_EQ                 { Command.Predicate.Eq }
| TK_LST                { Command.Predicate.Lst }
| TK_GST                { Command.Predicate.Gst }
| TK_LEQ                { Command.Predicate.Leq }
| TK_GEQ                { Command.Predicate.Geq }

expression:
| TK_INT                { Command.Expression.Cst $1 }
| TK_STR                { Command.Expression.Var $1 }
| TK_LPAREN expression TK_RPAREN
  { $2 }
| e=expression o=funop f=expression
  { Command.Expression.Op (e, o, f) }
| TK_SUB expression %prec UMINUS
  { match $2 with
    | Command.Expression.Cst c ->
       Command.Expression.Cst (- c)
    | e ->
       Command.Expression.Op
         (Command.Expression.Cst 0, Command.Expression.Sub, e) }

%inline funop:
| TK_ADD                { Command.Expression.Add }
| TK_SUB                { Command.Expression.Sub }
| TK_MUL                { Command.Expression.Mul }
| TK_DIV                { Command.Expression.Div }
