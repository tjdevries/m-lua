{
open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+

rule read =
  parse
  | white { read lexbuf }
  | "true" { TRUE }
  | "false" { FALSE }
  | "(" { LPAR }
  | ")" { RPAR }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { STAR }
  | "/" { SLASH }
  | "==" { EQ }
  | "=" { EQUAL }
  | int { INTEGER (int_of_string (Lexing.lexeme lexbuf)) }
  | id { NAME (Lexing.lexeme lexbuf) }
  | eof { EOF }
