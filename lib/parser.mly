%{
open Ast
%}

%token <int> INTEGER
%token <float> FLOAT
%token <Name.t> NAME
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
%token CONCAT
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
%token LOCAL
%token FUNCTION
%token RETURN
%token BREAK
%token END
%token FOR
%token WHILE
%token REPEAT
%token THEN
%token IF
%token ELSE
%token ELSEIF
%token IN
%token UNTIL
%token DO

%nonassoc NO_ARG
%nonassoc LPAR
(* %left OR *)
(* %left AND *)
(* %left LT GT LTE GTE NEQ EQ *)
(* (* %right DO *) *)
(* %left PLUS MINUS *)
(* %left STAR SLASH MOD *)
(* %left NOT OCTOTHORPE *)
(* %right EXP *)

%token EOF

%start <statement list> prog
%start <expr> one_exp
%%

(* Entry Points *)
let prog := terminated(terminated(stat, SEMICOLON?)*, EOF)
let one_exp := terminated(exp, EOF)

(* chunk ::= {stat [`;´]} [laststat[`;´]] *)
let chunk :=
  | statements = terminated(stat, SEMICOLON?)*;
    last_statement = terminated(laststat, SEMICOLON?)?;
    { { statements; last_statement } }

(* block ::= chunk *)
let block := chunk

(* stat ::= varlist1 `=´ explist1  |
            functioncall  |
            do block end  |
            while exp do block end  |
            repeat block until exp  |
            if exp then block {elseif exp then block} [else block] end  |
            for Name `=´ exp `,´ exp [`,´ exp] do block end  |
            for namelist in explist1 do block end  |
            function funcname funcbody  |
            local function Name funcbody  |
            local namelist [`=´ explist1] *)
let stat :=
  | binding
  | functioncall_statement
  | do_block
  | while_statement
  | repeat_statement
  | if_statement
  | for_range
  | for_expr
  | function_statement
  | local_function
  | local_namelist

let binding :=
  | v = varlist1; EQUAL; e = explist1; { Binding (v, e) }

let functioncall_statement :=
  | ~= functioncall; %prec NO_ARG <CallStatement>

let do_block :=
  | ~= delimited(DO, block, END); <Do>

let while_statement :=
  | WHILE; while_condition = exp; DO; while_block = block; END; { While { while_condition; while_block; } }

let repeat_statement := 
  | REPEAT; repeat_block = block; UNTIL; repeat_condition = exp; { Repeat { repeat_condition; repeat_block } }

let local_namelist := 
  | LOCAL; names = namelist; exprs = preceded(EQUAL, explist1)?; { LocalBinding { names; exprs = Ast.exprlist_opt exprs } }

let local_function := 
  | LOCAL; FUNCTION; local_name = NAME; (function_parameters, function_block) = funcbody;
      { LocalFunction { local_name; function_parameters; function_block } }

(* laststat ::= return [explist1]  |  break *)
let laststat :=
  | ~= preceded(RETURN, explist); <Return>
  | BREAK; { Break }

let function_statement :=
  | FUNCTION; function_name = funcname; (function_parameters, function_block) = funcbody; { 
    FunctionStatement { function_name; function_parameters; function_block }
  }

(* funcname ::= Name {`.´ Name} [`:´ Name] *)
let funcname :=
  | base = NAME;
    keys = preceded(DOT, NAME)*;
    table_method = preceded(COLON, NAME)?; { { base; keys; table_method } }

(* funcbody ::= `(´ [parlist1] `)´ block end *)
let funcbody :=
  | LPAR; p = parlist1?; RPAR; block = block; END; { Ast.parlist_opt p, block }

let for_expr :=
  | FOR; names = namelist; IN; exprs = explist1; DO; for_block = block; END; {
    ForNames { names; exprs; for_block }
  }

let for_range :=
  | FOR; name = NAME; EQUAL; start = exp; COMMA; finish = exp; step = preceded(COMMA, exp)?; DO;
      for_block = block;
    END;
      { ForRange { name; start; finish; step; for_block } }

let if_statement :=
  | IF; if_condition = exp; THEN; 
      if_block = block;
      elseifs = elseif_helper*;
      else_block = else_helper;
    END; 
      { If { conditions = [if_condition, if_block] @ elseifs @ else_block } }


let elseif_helper ==
  | ELSEIF; condition = exp; THEN; block = block; { (condition, block) }

let else_helper ==
  | { [] }
  | ELSE; block = block; { [ True, block ] }

let varlist1 :=
  |  separated_nonempty_list(COMMA, var)

let explist := separated_list(COMMA, exp)
let explist1 := separated_nonempty_list(COMMA, exp)

(* a | a, b | a, b, c  *)
let namelist :=
  | l = separated_nonempty_list(COMMA, NAME); { l }


(* parlist1 ::= namelist [`,´ `...´]  |  `...´ *)
let parlist1 :=
  | n = namelist; tr = TR_ELLIPSIS?; { { args = n; varargs = Option.is_some tr }  }
  | ELLIPSIS; { { args = []; varargs = true } }

(* tableconstructor ::= `{´ [fieldlist] `}´ *)
let tableconstructor :=
  | LBRACE; fieldlist = fieldlist?; RBRACE; { Table (Option.value ~default:[] fieldlist) }

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

let exp := exp_or

let exp_or := 
   | e1 = exp_or; OR; e2 = exp_and; { Or(e1, e2) }
   | exp_and

let exp_and :=
   | e1 = exp_and; AND; e2 = exp_logic;  { And(e1, e2) }
   | exp_logic

let exp_logic := 
   | e1 = exp_logic; LT;  e2 = exp_concat;  { LT(e1, e2) }
   | e1 = exp_logic; LTE; e2 = exp_concat;  { LTE(e1, e2) }
   | e1 = exp_logic; GT;  e2 = exp_concat;  { GT(e1, e2) }
   | e1 = exp_logic; GTE; e2 = exp_concat;  { GTE(e1, e2) }
   | e1 = exp_logic; EQ;  e2 = exp_concat;  { EQ(e1, e2) }
   | e1 = exp_logic; NEQ; e2 = exp_concat;  { NEQ(e1, e2) }
   | exp_concat

let exp_concat :=
  | e1 = exp_add; CONCAT; e2 = exp_concat; { Concat(e1, e2) }
  | exp_add

(* TODO: Add concats *)

let exp_add :=
   | e1 = exp_add; PLUS;  e2 = exp_mul; { Add(e1, e2) }
   | e1 = exp_add; MINUS; e2 = exp_mul; { Sub(e1, e2) }
   | exp_mul

let exp_mul :=
   | e1 = exp_mul; STAR;  e2 = exp_unop; { Mul(e1, e2) }
   | e1 = exp_mul; SLASH; e2 = exp_unop; { Div(e1, e2) }
   | e1 = exp_mul; MOD;   e2 = exp_unop; { Mod(e1, e2) }
   | exp_unop

let exp_unop :=
   | NOT; exp = exp_exponent; { Not exp }
   | OCTOTHORPE; exp = exp_exponent; { Len exp }
   | MINUS; exp = exp_exponent;  { Neg exp }
   | exp_exponent

let exp_exponent :=
  | e1 = exp_atom; EXP; e2 = exp_exponent; { Exponent(e1, e2) }
  | exp_atom

(* exp ::=  nil  |  false  |  true  |  Number  |  String  |  `...´  |*)
(*         function  |  prefixexp  |  tableconstructor  |  exp binop exp  |  unop exp*)
let exp_atom :=
 | NIL; { Nil $startpos }
 | TRUE; { True }
 | FALSE; { False }
 | i = INTEGER; { Number (Float.of_int(i)) }
 | f = FLOAT; { Number f }
 | s = STRING; { String s }
 | ELLIPSIS; { VarArgs }
 | FUNCTION; LPAR; p = parlist1?; RPAR; block = block; END; {
   let parameters = match p with
   | Some p -> p
   | None -> { args = []; varargs = false }
   in
   Function { parameters; block }
 }
 | ~=prefixexp; <> %prec NO_ARG
 | tableconstructor

(* args ::=  `(´ [explist1] `)´  |  tableconstructor  |  String *)
let args :=
   | delimited(LPAR, explist, RPAR)
   | t = tableconstructor; { [t] }
   | s = STRING; { [ String s ] }

(* functioncall ::=  prefixexp args  |  prefixexp `:´ Name args *)
let functioncall :=
   | prefix = prefixexp; args = args; { Call { prefix; args } }
   | prefix = prefixexp; COLON; name = NAME; args = args; { Self { prefix; name; args } }

(* prefixexp ::= var  |  functioncall  |  `(´ exp `)´*)
let prefixexp ==
   | var
   | ~= functioncall; <CallExpr>
   | LPAR; e = exp; RPAR; { e }

(* var ::=  Name  |  prefixexp `[´ exp `]´  |  prefixexp `.´ Name *)
let var :=
   | n = NAME; { Name n }
   | p = prefixexp; LBRACKET; e = exp; RBRACKET; { Index (p, e) }
   | p = prefixexp; DOT; n = NAME; { Dot (p, n) }

