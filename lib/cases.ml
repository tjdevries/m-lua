open Core

let%expect_test "maths" =
  Fmt.pr "basic: %a" Ast.pp_expr (Parse.parse_expr "(5 + 5)");
  [%expect {| basic: (Binop ((Integer 5), Add, (Integer 5))) |}];
  Fmt.pr "precedence: %a" Ast.pp_expr (Parse.parse_expr "(1 + 2 / 3)");
  [%expect
    {|
    precedence: (Binop ((Integer 1), Add, (Binop ((Integer 2), Div, (Integer 3)))
                   )) |}];
  ()
;;

let print_statements str =
  Fmt.pr "========@.";
  Parse.parse str |> List.iter ~f:(Fmt.pr "%a@." Ast.pp_statement)
;;

let%expect_test "globals" =
  print_statements "x = 5";
  [%expect {|
    ========
    (Binding (["x"], [(Integer 5)])) |}];
  ()
;;

(*let%expect_test "locals" =*)
(*  Fmt.pr "1: %a" Ast.pp_expr (Parse.parse "local x = 5");*)
(*  [%expect {| |}];*)
(*  ()*)
(*;;*)
