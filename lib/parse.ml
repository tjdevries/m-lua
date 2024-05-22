let lex (s : string) : Parser.token list =
  let lexbuf = Lexing.from_string s in
  let rec loop tokens =
    match Lexer.read lexbuf with
    | EOF -> List.rev tokens
    | token -> loop (token :: tokens)
  in
  loop []
;;

module I = Parser.MenhirInterpreter

let succeed v = Ok v

let fail lexbuf _ =
  let msg =
    Fmt.str
      "At offset %d: syntax error.\n%!"
      (Lexing.lexeme_start lexbuf)
  in
  Error msg
;;

(* type tok_loc = *)
(*   { token : Parser.token *)
(*   ; start_loc : Lexing.position *)
(*   ; end_loc : Lexing.position *)
(*   } *)
(* { token *)
(* ; start_loc = buf.lex_start_p *)
(* ; end_loc = buf.lex_curr_p *)
(* } *)

let loop lexbuf result =
  let rec skip_comments buf =
    match Lexer.read buf with
    | COMMENT _ -> skip_comments buf
    | LONG_COMMENT _ -> skip_comments buf
    | token -> token
  in
  let supplier =
    I.lexer_lexbuf_to_supplier skip_comments lexbuf
  in
  I.loop_handle succeed (fail lexbuf) supplier result
;;

let parse_with incremental s =
  let lexbuf = Lexing.from_string s in
  let parser = incremental lexbuf.lex_curr_p in
  match loop lexbuf parser with
  | Ok ast -> ast
  | Error msg -> Fmt.failwith "parse failed: %s" msg
;;

let parse s = parse_with Parser.Incremental.prog s
let parse_expr s = parse_with Parser.Incremental.one_exp s
