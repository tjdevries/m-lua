{
open Parser
}

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let int = '-'? digit+
let float = '-'? digit+ "." digit+
let letter = ['a'-'z' 'A'-'Z' '_']
let id = letter (letter | digit)*

(* Need to handle [==[ string inside ]==], this kind of stuff *)
let string = '"' [^'"']* '"'

let tr_ellipsis = ',' white '.' '.' '.'

rule read =
  parse
  | white { read lexbuf }
  | "nil" { NIL }
  | "true" { TRUE }
  | "false" { FALSE }
  | "(" { LPAR }
  | ")" { RPAR }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "," { COMMA }
  | ";" { SEMICOLON }
  | ".." { CONCAT }
  | "." { DOT }
  | "#" { OCTOTHORPE }
  | ":" { COLON }
  | "=" { EQUAL }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { STAR }
  | "/" { SLASH }
  | "^" { EXP }
  | "%" { MOD }
  | "<" { LT }
  | "<=" { LTE }
  | ">" { GT }
  | ">=" { GTE }
  | "==" { EQ }
  | "~=" { NEQ }
  | "and" { AND }
  | "or" { OR }
  | "not" { NOT }
  | "local" { KW_LOCAL }
  | "return" { KW_RETURN }
  | "break" { KW_BREAK }
  | "function" { KW_FUNCTION }
  | "end" { KW_END }
  | "for" { KW_FOR }
  | "while" { KW_WHILE }
  | "repeat" { KW_REPEAT }
  | "then" { KW_THEN }
  | "if" { KW_IF }
  | "else" { KW_ELSE }
  | "elseif" { KW_ELSEIF }
  | "in" { KW_IN }
  | "until" { KW_UNTIL }
  | "do" { KW_DO }
  | tr_ellipsis { TR_ELLIPSIS }
  | "..." { ELLIPSIS }
  | int { INTEGER (int_of_string (Lexing.lexeme lexbuf)) }
  | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | string { STRING (Lexing.lexeme lexbuf) }
  | id { NAME (Ast.Name.of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }
