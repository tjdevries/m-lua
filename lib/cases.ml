open Core

let%expect_test "maths" =
  Fmt.pr "basic: %a" Ast.pp_expr (Parse.parse_expr "(5 + 5)");
  [%expect {| basic: (Add ((Number 5.), (Number 5.))) |}];
  Fmt.pr "float: %a" Ast.pp_expr (Parse.parse_expr "3.14");
  [%expect {| float: (Number 3.14) |}];
  Fmt.pr "precedence: %a" Ast.pp_expr (Parse.parse_expr "(1 + 2 / 3)");
  [%expect
    {|
    precedence: (Add ((Number 1.), (Div ((Number 2.), (Number 3.))))) |}];
  ()
;;

let print_expr str = Fmt.pr "expr: %a" Ast.pp_expr (Parse.parse_expr str)

let%expect_test "unary" =
  print_expr "{ not true, -5, -hello, #{1, 2, 3}, -yes + 7, #\"hello\" }";
  [%expect
    {|
    expr: (Table
             [{ key = None; value = (Not True) };
               { key = None; value = (Number -5.) };
               { key = None; value = (Neg (Name "hello")) };
               { key = None;
                 value =
                 (Len
                    (Table
                       [{ key = None; value = (Number 1.) };
                         { key = None; value = (Number 2.) };
                         { key = None; value = (Number 3.) }]))
                 };
               { key = None; value = (Add ((Neg (Name "yes")), (Number 7.))) };
               { key = None; value = (Len (String "\"hello\"")) }])
    |}];
  ()
;;

let%expect_test "strings" =
  print_expr {| "hello" |};
  [%expect {| expr: (String "\"hello\"") |}];
  print_expr "true + false + nil";
  [%expect {| expr: (Add ((Add (True, False)), Nil)) |}];
  print_expr {| function(a) return a end |};
  [%expect
    {|
    expr: (Function
             { parameters = { args = ["a"]; varargs = false };
               block = [(Return (Name "a"))] }) |}];
  print_expr {| function(a) return a + a end |};
  [%expect
    {|
    expr: (Function
             { parameters = { args = ["a"]; varargs = false };
               block = [(Return (Add ((Name "a"), (Name "a"))))] }) |}];
  print_expr {| function(a, b) return a + b end |};
  [%expect
    {|
    expr: (Function
             { parameters = { args = ["a"; "b"]; varargs = false };
               block = [(Return (Add ((Name "a"), (Name "b"))))] }) |}];
  print_expr {| function(a, b, ...) return a + b end |};
  [%expect
    {|
    expr: (Function
             { parameters = { args = ["a"; "b"]; varargs = true };
               block = [(Return (Add ((Name "a"), (Name "b"))))] }) |}];
  ()
;;

let%expect_test "functions" =
  print_expr "f()";
  [%expect {| expr: FunctionCall {prefix = (Name "f"); args = []} |}];
  print_expr {| f "hello" |};
  [%expect
    {| expr: FunctionCall {prefix = (Name "f"); args = [(String "\"hello\"")]} |}];
  print_expr {| f {} |};
  [%expect {| expr: FunctionCall {prefix = (Name "f"); args = [(Table [])]} |}];
  print_expr {| f { x = 5 } |};
  [%expect
    {|
    expr: FunctionCall {prefix = (Name "f");
            args = [(Table [{ key = (Some (Name "x")); value = (Number 5.) }])]} |}];
  print_expr {| f { 1, 2; 3, key = 4; } |};
  [%expect
    {|
    expr: FunctionCall {prefix = (Name "f");
            args =
            [(Table
                [{ key = None; value = (Number 1.) };
                  { key = None; value = (Number 2.) };
                  { key = None; value = (Number 3.) };
                  { key = (Some (Name "key")); value = (Number 4.) }])
              ]} |}];
  print_expr {| t:name(1, 2, "last_arg") |};
  [%expect
    {|
    expr: TableCall {prefix = (Name "t"); name = "name";
            args = [(Number 1.); (Number 2.); (String "\"last_arg\"")]} |}];
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
    (Binding ([(Name "x")], [(Number 5.)])) |}];
  ()
;;

(*let%expect_test "locals" =*)
(*  Fmt.pr "1: %a" Ast.pp_expr (Parse.parse "local x = 5");*)
(*  [%expect {| |}];*)
(*  ()*)
(*;;*)
