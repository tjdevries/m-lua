{
open Parser
}

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let float = '-'? digit+ ("." digit+)?
let letter = ['a'-'z' 'A'-'Z' '_']
let id = letter (letter | digit)*

(* Need to handle [==[ string inside ]==], this kind of stuff *)
let str1 = '\"' ([^ '\"' '\\' ] | ([ '\\' ] [^ '_' ]))* '\"'
let str2 = '\'' ([^ '\'' '\\' ] | ([ '\\' ] [^ '_' ]))* '\''
let string = str1 | str2
let lstr = '[' '='* '['
let lcomm = '-' '-' lstr
let lend = ']' '='*

let tr_ellipsis = ',' white '.' '.' '.'

rule read =
  parse
  | white { read lexbuf }
  | "--[[" { longcomment lexbuf }
  | "--" { shortcomment lexbuf }
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
  | "local" { LOCAL }
  | "return" { RETURN }
  | "break" { BREAK }
  | "function" { FUNCTION }
  | "end" { END }
  | "for" { FOR }
  | "while" { WHILE }
  | "repeat" { REPEAT }
  | "then" { THEN }
  | "if" { IF }
  | "else" { ELSE }
  | "elseif" { ELSEIF }
  | "in" { IN }
  | "until" { UNTIL }
  | "do" { DO }
  | tr_ellipsis { TR_ELLIPSIS }
  | "..." { ELLIPSIS }
  | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | string { 
      let quoted_string = Lexing.lexeme lexbuf in
      STRING (String.sub quoted_string 1 (String.length quoted_string - 2))
    }
  | id { NAME (Name.of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }

and longcomment =
  parse
  | "]]" { LONG_COMMENT "KEKW" }
  | eof  { failwith "EOF in longcomment" }
  | _    { longcomment lexbuf }

and shortcomment =
  parse
  | '\n'  { COMMENT "SHORT" }
  | _     { shortcomment lexbuf }

{
let token_to_string (t : Parser.token) : string =
  let open Parser in
  match t with
  | FLOAT f -> Fmt.str "Float(%f)" f
  | NAME s -> Fmt.str "Name(%s)" s
  | STRING s -> Fmt.str "String(%s)" s
  | COMMENT s -> Fmt.str "COMMENT(%s)" s
  | LONG_COMMENT s -> Fmt.str "LONG_COMMENT(%s)" s
  | NIL -> "NIL "
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | ELLIPSIS -> "ELLIPSIS"
  | TR_ELLIPSIS -> "TR_ELLIPSIS"
  | LPAR -> "LPAR"
  | RPAR -> "RPAR"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | LBRACKET -> "LBRACKET"
  | RBRACKET -> "RBRACKET"
  | DOT -> "DOT"
  | OCTOTHORPE -> "OCTOTHORPE"
  | CONCAT -> "CONCAT"
  | COLON -> "COLON"
  | EQUAL -> "EQUAL"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | STAR -> "STAR"
  | SLASH -> "SLASH"
  | EXP -> "EXP"
  | MOD -> "MOD"
  | LT -> "LT"
  | LTE -> "LTE"
  | GT -> "GT"
  | GTE -> "GTE"
  | EQ -> "EQ"
  | NEQ -> "NEQ"
  | AND -> "AND"
  | NOT -> "NOT"
  | OR -> "OR"
  | COMMA -> "COMMA"
  | SEMICOLON -> "SEMICOLON"
  | LOCAL -> "LOCAL"
  | FUNCTION -> "FUNCTION"
  | RETURN -> "RETURN"
  | BREAK -> "BREAK"
  | END -> "END"
  | FOR -> "FOR"
  | WHILE -> "WHILE"
  | REPEAT -> "REPEAT"
  | THEN -> "THEN"
  | IF -> "IF"
  | ELSE -> "ELSE"
  | ELSEIF -> "ELSEIF"
  | IN -> "IN"
  | UNTIL -> "UNTIL"
  | DO -> "DO"
  | _ -> failwith "token_to_string"
;;
}
