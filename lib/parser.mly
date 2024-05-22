%{
open Ast
%}

%token <float> FLOAT
%token <Name.t> NAME
%token <string> STRING
%token <string> COMMENT
%token <string> LONG_COMMENT

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

(* varlist1 `=´ explist1 *)
let binding :=
  | ~ = varlist1; EQUAL; ~ = explist1; <Binding>

(* functioncall *)
let functioncall_statement :=
  | ~ = functioncall; %prec NO_ARG <CallStatement>

(* do block end  *)
let do_block :=
  | ~ = delimited(DO, block, END); <Do>

(* while exp do block end *)
let while_statement :=
  | WHILE; ~ = exp; DO; ~ = block; END; <While>

(* repeat block until exp *)
let repeat_statement :=
  | REPEAT; ~ = block; UNTIL; ~ = exp; <Repeat>

(* if exp then block {elseif exp then block} [else block] end  | *)
let if_statement :=
  | IF; if_condition = exp; THEN; 
      if_block = block;
      elseifs = elseif_helper*;
      else_block = else_helper;
    END; 
      { If ([if_condition, if_block] @ elseifs @ else_block) }

let elseif_helper ==
  | ELSEIF; condition = exp; THEN; block = block; { (condition, block) }

let else_helper ==
  | { [] }
  | ELSE; block = block; { [ True, block ] }

(* for namelist in explist1 do block end *)
let for_expr :=
  | FOR; ~ = namelist; IN; ~ = explist1; DO; ~ = block; END; <ForNames>

(* for Name `=´ exp `,´ exp [`,´ exp] do block end *)
let for_range :=
  | FOR; name = NAME; EQUAL; start = exp; COMMA; finish = exp; step = preceded(COMMA, exp)?; DO;
      for_block = block;
    END;
      { ForRange { name; start; finish; step; for_block } }

(* function funcname funcbody *)
let function_statement :=
  | FUNCTION; function_name = funcname; (function_parameters, function_block) = funcbody;
      { FunctionStatement { function_name; function_parameters; function_block } }

(* local function Name funcbody *)
let local_function := 
  | LOCAL; FUNCTION; local_name = NAME; (function_parameters, function_block) = funcbody;
      { LocalFunction { local_name; function_parameters; function_block } }

(* local namelist [`=´ explist1] *)
let local_namelist := 
  | LOCAL; ~ = namelist; ~ = or_list(preceded(EQUAL, explist1)); <LocalBinding>

(* laststat ::= return [explist1]  |  break *)
let laststat :=
  | ~ = preceded(RETURN, explist); <Return>
  | BREAK; { Break }

(* funcname ::= Name {`.´ Name} [`:´ Name] *)
let funcname :=
  | base = NAME;
    keys = preceded(DOT, NAME)*;
    table_method = preceded(COLON, NAME)?; { { base; keys; table_method } }

(* funcbody ::= `(´ [parlist1] `)´ block end *)
let funcbody :=
  | p = delimited(LPAR, parlist1?, RPAR); ~ = block; END; { Ast.parlist_opt p, block }

let varlist1 :=
  |  separated_nonempty_list(COMMA, var)

let explist :=
  | separated_list(COMMA, exp)

(* explist1 ::= {exp `,´} exp *)
let explist1 :=
  | separated_nonempty_list(COMMA, exp)

(* namelist ::= Name {`,´ Name} *)
let namelist :=
  | separated_nonempty_list(COMMA, NAME)

(* parlist1 ::= namelist [`,´ `...´]  |  `...´ *)
let parlist1 :=
  | n = namelist; tr = TR_ELLIPSIS?; { { args = n; varargs = Option.is_some tr }  }
  | ELLIPSIS; { { args = []; varargs = true } }

(* tableconstructor ::= `{´ [fieldlist] `}´ *)
let tableconstructor :=
  | ~ = delimited(LBRACE, or_list(fieldlist), RBRACE); <Table>

(* fieldlist ::= field {fieldsep field} [fieldsep] *)
let fieldlist :=
  | cons(field, trailing_fieldlist)

let trailing_fieldlist :=
  | (* nothing *) { [] }
  | fieldsep; { [] }
  | fieldsep; cons(field, trailing_fieldlist)

(* field ::= `[´ exp `]´ `=´ exp  |  Name `=´ exp  |  exp *)
let field :=
  | key = delimited(LBRACKET, exp, RBRACKET); EQUAL; value = exp; { Some key, value }
  | ~ = name; EQUAL; ~ = exp; { Some (String name), exp }
  | ~ = exp; { None, exp }

(* fieldsep ::= `,´  |  `;´ *)
let fieldsep :=
  | COMMA
  | SEMICOLON

let exp := exp_or

let exp_or := 
   | ~ = exp_or; OR; ~ = exp_and; <Or>
   | exp_and

let exp_and :=
   | ~ = exp_and; AND; ~ = exp_logic; <And>
   | exp_logic

let exp_logic := 
   | ~ = exp_logic; LT;  ~ = exp_concat; <LT>
   | ~ = exp_logic; LTE; ~ = exp_concat; <LTE>
   | ~ = exp_logic; GT;  ~ = exp_concat; <GT>
   | ~ = exp_logic; GTE; ~ = exp_concat; <GTE>
   | ~ = exp_logic; EQ;  ~ = exp_concat; <EQ>
   | ~ = exp_logic; NEQ; ~ = exp_concat; <NEQ>
   | exp_concat

let exp_concat :=
  | ~ = exp_add; CONCAT; ~ = exp_concat; <Concat>
  | exp_add

(* TODO: Add concats *)

let exp_add :=
   | ~ = exp_add; PLUS;  ~ = exp_mul; <Add>
   | ~ = exp_add; MINUS; ~ = exp_mul; <Sub>
   | exp_mul

let exp_mul :=
   | ~ = exp_mul; STAR;  ~ = exp_unop; <Mul>
   | ~ = exp_mul; SLASH; ~ = exp_unop; <Div>
   | ~ = exp_mul; MOD;   ~ = exp_unop; <Mod>
   | exp_unop


let exp_unop :=
   | NOT; ~ = exp_exponent; <Not>
   | OCTOTHORPE; ~ = exp_exponent; <Len>
   | MINUS; ~ = exp_exponent; <Neg>
   | exp_exponent

let exp_exponent :=
  | ~ = exp_atom; EXP; ~ = exp_exponent; <Exponent>
  | exp_atom

(* exp ::=  nil  |  false  |  true  |  Number  |  String  |  `...´  |*)
(*         function  |  prefixexp  |  tableconstructor  |  exp binop exp  |  unop exp*)
let exp_atom :=
 | NIL; { Nil }
 | TRUE; { True }
 | FALSE; { False }
 | ELLIPSIS; { VarArgs }
 | float
 | string
 | exp_function
 | exp_prefix
 | tableconstructor

(* args ::=  `(´ [explist1] `)´  |  tableconstructor  |  String *)
let args :=
   | delimited(LPAR, explist, RPAR)
   | to_list(tableconstructor)
   | to_list(string)

(* functioncall ::=  prefixexp args  |  prefixexp `:´ Name args *)
let functioncall :=
   | ~ = prefixexp; ~ = args; <Call>
   | ~ = prefixexp; COLON; ~ = name; ~ = args; <Self>

(* prefixexp ::= var  |  functioncall  |  `(´ exp `)´*)
let prefixexp ==
   | var
   | ~ = functioncall; <CallExpr>
   | delimited(LPAR, exp, RPAR)

(* var ::=  Name  |  prefixexp `[´ exp `]´  |  prefixexp `.´ Name *)
let var :=
   | ~ = name; <Name>
   | ~ = prefixexp; LBRACKET; ~ = exp; RBRACKET; <Index>
   | ~ = prefixexp; DOT; ~ = name; <Dot>

let name :=
   | ~ = NAME; <>

let string == 
   | ~ = STRING; <String>

let float ==
   | ~ = FLOAT; <Number>

let exp_function ==
   | FUNCTION; LPAR; ~ = parameters; RPAR; ~ = block; END; { Function { parameters; block } }

let parameters ==
  | p = parlist1?; { Ast.parlist_opt p }

let exp_prefix ==
   | ~ = prefixexp; <> %prec NO_ARG

let to_list(rule) ==
   | rule = rule; { [rule] }

let cons(hd, rest) ==
  | ~ = hd; ~ = rest; { hd :: rest }

let or_list(rule) ==
   | { [] }
   | rule

