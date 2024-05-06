%{
open Ast
%}

%token <int> INTEGER
%token <float> FLOAT
%token <Ast.Name.t> NAME
%token <string> STRING

%token NIL
%token TRUE
%token FALSE
%token ELLIPSIS
%token TR_ELLIPSIS
%token LPAR
%token RPAR
%token LBRACE
%token RBRACE
%token LBRACKET
%token RBRACKET
%token DOT
%token OCTOTHORPE
%token COLON
%token EQUAL
%token PLUS
%token MINUS
%token STAR
%token SLASH
%token EXP
%token MOD
%token LT
%token LTE
%token GT
%token GTE
%token EQ
%token NEQ
%token AND
%token NOT
%token OR
%token COMMA
%token SEMICOLON
%token KW_FUNCTION
%token KW_RETURN
%token KW_END
%token KW_FOR
%token KW_WHILE
%token KW_REPEAT
%token KW_THEN
%token KW_IF
%token KW_ELSE
%token KW_ELSEIF
%token KW_IN
%token KW_UNTIL
%token KW_DO

(*%token SEMICOLON ";"*)

%token EOF


%start <statement list> prog
%start <expr> one_exp
%%

prog:
 (*| s = stat; EOF { s }*)
 (*| s = separated_list(stat, SEMICOLON); EOF { s }*)
 | s = list(stat); EOF { s }
 ;

one_exp:
  | e = exp; EOF { e }
  ;

let stat :=
  | v = varlist1; EQUAL; e = explist1; { Binding (v, e) }
  | f = functioncall; { Call f }
  | KW_DO; do_block = block; KW_END; { Do { do_block } }
  | KW_WHILE; while_condition = exp; KW_DO; while_block = block; { While { while_condition; while_block; } }
  | KW_REPEAT; repeat_block = block; KW_UNTIL; repeat_condition = exp; { Repeat { repeat_condition; repeat_block } }
  (* IF *)
  | KW_RETURN; e = exp; { Return e }

varlist1:
  | n = NAME { [Name n] }
  ;

let explist1 :=
  | e = separated_nonempty_list(COMMA, exp); { e }

let block :=
  | s = list(stat); { s }

(* a | a, b | a, b, c  *)
let namelist :=
  | l = separated_nonempty_list(COMMA, NAME); { l }


(* parlist1 ::= namelist [`,´ `...´]  |  `...´ *)
let parlist1 :=
  | n = namelist; tr = option(TR_ELLIPSIS); { { args = n; varargs = Option.is_some tr }  }
  | ELLIPSIS; { { args = []; varargs = true } }

(* tableconstructor ::= `{´ [fieldlist] `}´ *)
let tableconstructor :=
  | LBRACE; fieldlist = option(fieldlist); RBRACE; { Table (Option.value ~default:[] fieldlist) }

(* fieldlist ::= field {fieldsep field} [fieldsep] *)
let fieldlist :=
  | field = field; fields = trailing_fieldlist; { field :: fields }

let trailing_fieldlist :=
  | (* nothing *) { [] }
  | fieldsep; field = field; rest = trailing_fieldlist;   { field :: rest }
  | fieldsep; { [] }

(* field ::= `[´ exp `]´ `=´ exp  |  Name `=´ exp  |  exp *)
let field :=
  | LBRACKET; key = exp; RBRACKET; EQUAL; value = exp; { { key = Some key; value } }
  | name = NAME; EQUAL; value = exp; { { key = Some (Name name); value } }
  | value = exp; { { key = None; value } }

(* fieldsep ::= `,´  |  `;´ *)
let fieldsep :=
  | COMMA
  | SEMICOLON

let exp := exp_logic

let exp_logic := 
   | e1 = exp_logic; LT;  e2 = exp_maths;  { LT(e1, e2) }
   | e1 = exp_logic; LTE; e2 = exp_maths;  { LTE(e1, e2) }
   | e1 = exp_logic; GT;  e2 = exp_maths;  { GT(e1, e2) }
   | e1 = exp_logic; GTE; e2 = exp_maths;  { GTE(e1, e2) }
   | e1 = exp_logic; EQ;  e2 = exp_maths;  { EQ(e1, e2) }
   | e1 = exp_logic; NEQ; e2 = exp_maths;  { NEQ(e1, e2) }
   | e1 = exp_logic; AND; e2 = exp_maths;  { And(e1, e2) }
   | e1 = exp_logic; OR;  e2 = exp_maths;  { Or(e1, e2) }
   | exp_maths

let exp_maths :=
   | e1 = exp_maths; MOD; e2 = exp_add; { Mod(e1, e2) }
   | e1 = exp_maths; EXP; e2 = exp_add; { Exponent(e1, e2) }
   | exp_add

let exp_add :=
   | e1 = exp_add; PLUS;  e2 = exp_mul;  { Add(e1, e2) }
   | e1 = exp_add; MINUS; e2 = exp_mul; { Sub(e1, e2) }
   | exp_mul

let exp_mul :=
   | e1 = exp_mul; STAR;  e2 = exp_unop;  { Mul(e1, e2) }
   | e1 = exp_mul; SLASH; e2 = exp_unop; { Div(e1, e2) }
   | exp_unop

let exp_unop :=
   | MINUS; exp = exp_atom;  { Neg exp }
   | NOT; exp = exp_atom; { Not exp }
   | OCTOTHORPE; exp = exp_atom; { Len exp }
   | exp_atom

(* exp ::=  nil  |  false  |  true  |  Number  |  String  |  `...´  |*)
(*         function  |  prefixexp  |  tableconstructor  |  exp binop exp  |  unop exp*)
let exp_atom :=
 | NIL; { Nil }
 | TRUE; { True }
 | FALSE; { False }
 | i = INTEGER; { Number (Float.of_int(i)) }
 | f = FLOAT; { Number f }
 | s = STRING; { String s }
 | ELLIPSIS; { VarArgs }
 | KW_FUNCTION; LPAR; p = option(parlist1); RPAR; block = block; KW_END; {
   let parameters = match p with
   | Some p -> p
   | None -> { args = []; varargs = false }
   in
   Function { parameters; block }
 }
 | p = prefixexp; { p }
 | t = tableconstructor; { t }

(* args ::=  `(´ [explist1] `)´  |  tableconstructor  |  String *)
let args :=
   | LPAR; e = option(explist1); RPAR; {
       match e with
       | Some e -> e
       | None -> []
     }
   | t = tableconstructor; { [t] }
   | s = STRING; { [ String s ] }

(* functioncall ::=  prefixexp args  |  prefixexp `:´ Name args *)
let functioncall :=
   | prefix = prefixexp; args = args; { FunctionCall { prefix; args } }
   | prefix = prefixexp; COLON; name = NAME; args = args; { TableCall { prefix; name; args } }

(* prefixexp ::= var  |  functioncall  |  `(´ exp `)´*)
let prefixexp :=
   | v = var; { v }
   | f = functioncall; { f }
   | LPAR; e = exp; RPAR; { e }

(* var ::=  Name  |  prefixexp `[´ exp `]´  |  prefixexp `.´ Name *)
let var :=
   | n = NAME; { Name n }
   | p = prefixexp; LBRACKET; e = exp; RBRACKET; { Index (p, e) }
   | p = prefixexp; DOT; n = NAME; { Dot (p, n) }

