%{
open Ast
%}

%token <int> INTEGER
%token <string> NAME

%token TRUE
%token FALSE
%token LPAR
%token RPAR
%token PLUS
%token MINUS
%token EQUAL
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
%token OR
%token SEMICOLON ";"

%token EOF

%left PLUS MINUS
%left STAR SLASH

%start <statement list> prog
%start <expr> one_expr
%%

prog:
 (*| s = stat; EOF { s }*)
 (*| s = separated_list(stat, SEMICOLON); EOF { s }*)
 | s = stat; EOF { [ s ] }
 ;

one_expr:
  | e = expr; EOF { e }

stat:
  | v = varlist1; EQUAL; e = explist1; { Binding (v, e) }
  ;

varlist1:
  | n = NAME { [n] }

explist1:
  | e = expr; { [e]}

expr:
 | i = INTEGER { Integer i }
 | LPAR; e = expr; RPAR { e }
 | e1 = expr; PLUS; e2 = expr { Binop(e1, Add, e2) }
 | e1 = expr; MINUS; e2 = expr { Binop(e1, Sub, e2) }
 | e1 = expr; STAR; e2 = expr { Binop(e1, Mul, e2) }
 | e1 = expr; SLASH; e2 = expr { Binop(e1, Div, e2) }
 ;
