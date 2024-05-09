open Core

let%expect_test "maths" =
  Fmt.pr
    "basic: %a"
    Ast.pp_expr
    (Parse.parse_expr "(5 + 5)");
  [%expect {| basic: (Add ((Number 5.), (Number 5.))) |}];
  Fmt.pr "float: %a" Ast.pp_expr (Parse.parse_expr "3.14");
  [%expect {| float: (Number 3.14) |}];
  Fmt.pr
    "precedence: %a"
    Ast.pp_expr
    (Parse.parse_expr "(1 + 2 / 3)");
  [%expect
    {|
    precedence: (Add ((Number 1.), (Div ((Number 2.), (Number 3.))))) |}];
  ()
;;

let print_expr str =
  Fmt.pr "expr: %a" Ast.pp_expr (Parse.parse_expr str)
;;

let%expect_test "precedence" =
  print_expr
    {| { 1 + 2 + 3, 1 + 5 * 2 ^ 3, "hello" .. "middle" .. "world" } |};
  [%expect
    {|
    expr: (Table
             [{ key = None;
                value = (Add ((Add ((Number 1.), (Number 2.))), (Number 3.))) };
               { key = None;
                 value =
                 (Add ((Number 1.),
                    (Mul ((Number 5.), (Exponent ((Number 2.), (Number 3.)))))))
                 };
               { key = None;
                 value =
                 (Concat ((String "hello"),
                    (Concat ((String "middle"), (String "world")))))
                 }
               ]) |}];
  ()
;;

let%expect_test "unary" =
  print_expr
    "{ not true, -5, -hello, #{1, 2, 3}, -yes + 7, \
     #\"hello\" }";
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
               { key = None; value = (Len (String "hello")) }])
    |}];
  ()
;;

let%expect_test "expressions" =
  print_expr {| "hello" |};
  [%expect {| expr: (String "hello") |}];
  print_expr "true + false + nil";
  [%expect {| expr: (Add ((Add (True, False)), Nil)) |}];
  print_expr {| function(a) return a end |};
  [%expect
    {|
    expr: (Function
             { parameters = { args = ["a"]; varargs = false };
               block =
               { statements = []; last_statement = (Some (Return [(Name "a")])) }
               }) |}];
  print_expr {| function(a) return a + a end |};
  [%expect
    {|
    expr: (Function
             { parameters = { args = ["a"]; varargs = false };
               block =
               { statements = [];
                 last_statement =
                 (Some (Return [(Add ((Name "a"), (Name "a")))])) }
               }) |}];
  print_expr {| function(a, b) return a + b end |};
  [%expect
    {|
    expr: (Function
             { parameters = { args = ["a"; "b"]; varargs = false };
               block =
               { statements = [];
                 last_statement =
                 (Some (Return [(Add ((Name "a"), (Name "b")))])) }
               }) |}];
  print_expr {| function(a, b, ...) return a + b end |};
  [%expect
    {|
    expr: (Function
             { parameters = { args = ["a"; "b"]; varargs = true };
               block =
               { statements = [];
                 last_statement =
                 (Some (Return [(Add ((Name "a"), (Name "b")))])) }
               }) |}];
  ()
;;

let%expect_test "functions" =
  print_expr "f()";
  [%expect
    {| expr: (CallExpr Call {prefix = (Name "f"); args = []}) |}];
  print_expr {| f "hello" |};
  [%expect
    {| expr: (CallExpr Call {prefix = (Name "f"); args = [(String "hello")]}) |}];
  print_expr {| f {} |};
  [%expect
    {| expr: (CallExpr Call {prefix = (Name "f"); args = [(Table [])]}) |}];
  print_expr {| f { x = 5 } |};
  [%expect
    {|
    expr: (CallExpr
             Call {prefix = (Name "f");
               args =
               [(Table [{ key = (Some (Name "x")); value = (Number 5.) }])]}) |}];
  print_expr {| f { 1, 2; 3, key = 4; } |};
  [%expect
    {|
    expr: (CallExpr
             Call {prefix = (Name "f");
               args =
               [(Table
                   [{ key = None; value = (Number 1.) };
                     { key = None; value = (Number 2.) };
                     { key = None; value = (Number 3.) };
                     { key = (Some (Name "key")); value = (Number 4.) }])
                 ]}) |}];
  print_expr {| t:name(1, 2, "last_arg") |};
  [%expect
    {|
    expr: (CallExpr
             Self {prefix = (Name "t"); name = "name";
               args = [(Number 1.); (Number 2.); (String "last_arg")]}) |}];
  ()
;;

let print_statements str =
  Fmt.pr "========@.";
  Parse.parse str
  |> List.iter ~f:(Fmt.pr "%a@." Ast.pp_statement)
;;

let%expect_test "globals" =
  print_statements "x = 1; y = true";
  [%expect
    {|
    ========
    (Binding ([(Name "x")], [(Number 1.)]))
    (Binding ([(Name "y")], [True])) |}];
  ()
;;

let%expect_test "if" =
  print_statements "if true then print(5) end";
  [%expect
    {|
    ========
    If {
      conditions =
      [(True,
        { statements =
          [(CallStatement Call {prefix = (Name "print"); args = [(Number 5.)]})];
          last_statement = None })
        ]} |}];
  print_statements
    {| 
      if true then
        print(true)
      elseif false then
        print(false)
      else
        print("yaya")
      end
     |};
  [%expect
    {|
    ========
    If {
      conditions =
      [(True,
        { statements =
          [(CallStatement Call {prefix = (Name "print"); args = [True]})];
          last_statement = None });
        (False,
         { statements =
           [(CallStatement Call {prefix = (Name "print"); args = [False]})];
           last_statement = None });
        (True,
         { statements =
           [(CallStatement
               Call {prefix = (Name "print"); args = [(String "yaya")]})
             ];
           last_statement = None })
        ]} |}];
  ()
;;

let%expect_test "for" =
  print_statements
    {|
    for x = 1, 5 do
      print(x)
    end

    for y = 1, 10, 2 do
      print(y)
    end

    for idx, value in ipairs(mylist) do
      print(idx, value)
    end
  |};
  [%expect
    {|
    ========
    ForRange {name = "x"; start = (Number 1.); finish = (Number 5.); step = None;
      for_block =
      { statements =
        [(CallStatement Call {prefix = (Name "print"); args = [(Name "x")]})];
        last_statement = None }}
    ForRange {name = "y"; start = (Number 1.); finish = (Number 10.);
      step = (Some (Number 2.));
      for_block =
      { statements =
        [(CallStatement Call {prefix = (Name "print"); args = [(Name "y")]})];
        last_statement = None }}
    ForNames {names = ["idx"; "value"];
      exprs =
      [(CallExpr Call {prefix = (Name "ipairs"); args = [(Name "mylist")]})];
      for_block =
      { statements =
        [(CallStatement
            Call {prefix = (Name "print"); args = [(Name "idx"); (Name "value")]})
          ];
        last_statement = None }} |}];
  ()
;;

let%expect_test "functions" =
  print_statements
    {|
    function hi() print("hi") end

    function with.args(a, b, c) print(b, c, a) end
  |};
  [%expect
    {|
    ========
    FunctionStatement {
      function_name =
      { Ast.FuncName.base = "hi"; keys = []; table_method = None };
      function_parameters = { args = []; varargs = false };
      function_block =
      { statements =
        [(CallStatement Call {prefix = (Name "print"); args = [(String "hi")]})];
        last_statement = None }}
    FunctionStatement {
      function_name =
      { Ast.FuncName.base = "with"; keys = ["args"]; table_method = None };
      function_parameters = { args = ["a"; "b"; "c"]; varargs = false };
      function_block =
      { statements =
        [(CallStatement
            Call {prefix = (Name "print");
              args = [(Name "b"); (Name "c"); (Name "a")]})
          ];
        last_statement = None }} |}];
  ()
;;

(*let%expect_test "locals" =*)
(*  Fmt.pr "1: %a" Ast.pp_expr (Parse.parse "local x = 5");*)
(*  [%expect {| |}];*)
(*  ()*)
(*;;*)
