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
%token KW_LOCAL
%token KW_FUNCTION
%token KW_RETURN
%token KW_BREAK
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

(* %nonassoc LPAR *)
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
let prog := terminated(list(terminated(stat, option(SEMICOLON))), EOF)
let one_exp := terminated(exp, EOF)

(* chunk ::= {stat [`;´]} [laststat[`;´]] *)
let chunk :=
  | statements = terminated(list(terminated(stat, option(SEMICOLON))), option(SEMICOLON));
    last_statement = option(terminated(laststat, option(SEMICOLON)));
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
  | ~= functioncall; <CallStatement>
  | do_block
  | while_statement
  | repeat_statement
  | if_statement
  | for_range
  | for_expr
  | function_statement
  | local_function
  | local_namelist

let binding := v = varlist1; EQUAL; e = explist1; { Binding (v, e) }

let do_block := KW_DO; do_block = block; KW_END; { Do { do_block } }

let while_statement := KW_WHILE; while_condition = exp; KW_DO; while_block = block; KW_END; { While { while_condition; while_block; } }

let repeat_statement := KW_REPEAT; repeat_block = block; KW_UNTIL; repeat_condition = exp; {
    Repeat { repeat_condition; repeat_block }
  }

let local_namelist := KW_LOCAL; names = namelist; exprs = option(preceded(EQUAL, explist1)); {
  LocalBinding { names; exprs = Ast.exprlist_opt exprs }
}

let local_function := KW_LOCAL; KW_FUNCTION; local_name = NAME; (function_parameters, function_block) = funcbody; {
  LocalFunction { local_name; function_parameters; function_block }
}

(* laststat ::= return [explist1]  |  break *)
let laststat :=
  | ~= preceded(KW_RETURN, explist); <Return>
  | KW_BREAK; { Break }

let function_statement :=
  | KW_FUNCTION; function_name = funcname; (function_parameters, function_block) = funcbody; { 
    FunctionStatement { function_name; function_parameters; function_block }
  }

(* funcname ::= Name {`.´ Name} [`:´ Name] *)
let funcname :=
  | base = NAME;
    keys = list(preceded(DOT, NAME));
    table_method = preceded(COLON, NAME)?; { { base; keys; table_method } }

(* funcbody ::= `(´ [parlist1] `)´ block end *)
let funcbody :=
  | LPAR; p = parlist1?; RPAR; block = block; KW_END; { Ast.parlist_opt p, block }

let for_expr :=
  | KW_FOR; names = namelist; KW_IN; exprs = explist1; KW_DO; for_block = block; KW_END; {
    ForNames { names; exprs; for_block }
  }

let for_range :=
  | KW_FOR; name = NAME; EQUAL; start = exp; COMMA; finish = exp; step = preceded(COMMA, exp)?; KW_DO;
      for_block = block;
    KW_END;
      { ForRange { name; start; finish; step; for_block } }

let if_statement :=
  | KW_IF; if_condition = exp; KW_THEN; if_block = block;
    elseifs = list(elseif_helper);
    else_block = else_helper;
    KW_END; 
      { If { conditions = [if_condition, if_block] @ elseifs @ else_block } }


let elseif_helper :=
  | KW_ELSEIF; condition = exp; KW_THEN; block = block; { (condition, block) }

let else_helper :=
  | { [] }
  | KW_ELSE; block = block; { [ True, block ] }

let varlist1 :=
  | n = separated_nonempty_list(COMMA, NAME); { List.map (fun n -> Name n) n }

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
   | e1 = exp_logic; LT;  e2 = exp_add;  { LT(e1, e2) }
   | e1 = exp_logic; LTE; e2 = exp_add;  { LTE(e1, e2) }
   | e1 = exp_logic; GT;  e2 = exp_add;  { GT(e1, e2) }
   | e1 = exp_logic; GTE; e2 = exp_add;  { GTE(e1, e2) }
   | e1 = exp_logic; EQ;  e2 = exp_add;  { EQ(e1, e2) }
   | e1 = exp_logic; NEQ; e2 = exp_add;  { NEQ(e1, e2) }
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
  | e1 = exp_exponent; EXP; e2 = exp_atom; { Exponent(e1, e2) }
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
 | KW_FUNCTION; LPAR; p = parlist1?; RPAR; block = block; KW_END; {
   let parameters = match p with
   | Some p -> p
   | None -> { args = []; varargs = false }
   in
   Function { parameters; block }
 }
 | prefixexp
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

