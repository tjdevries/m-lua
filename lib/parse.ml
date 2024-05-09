let parse_expr (s : string) : Ast.expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.one_exp Lexer.read lexbuf in
  ast
;;

let parse (s : string) : Ast.program =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
;;
