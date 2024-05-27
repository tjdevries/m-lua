open Core

let print_expr str = Fmt.pr "expr: %a" Ast.pp_expr (Parse.parse_expr str)

let print_statements str =
  Fmt.pr "========@.";
  Parse.parse str |> List.iter ~f:(Fmt.pr "%a@." Ast.pp_statement)
;;

let%expect_test "globals" =
  print_statements "x = 1; y = true";
  [%expect
    {|
    ========
    (Binding ([(Name "x")], [1]))
    (Binding ([(Name "y")], [True])) |}];
  ()
;;

let%expect_test "if" =
  print_statements "if true then print(5) end";
  [%expect
    {|
    ========
    (If
       [(True,
         { statements =
           [(CallStatement (Call ((PrefixVar (Name "print")), [5])))];
           last_statement = None })
         ]) |}];
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
    (If
       [(True,
         { statements =
           [(CallStatement (Call ((PrefixVar (Name "print")), [True])))];
           last_statement = None });
         (False,
          { statements =
            [(CallStatement (Call ((PrefixVar (Name "print")), [False])))];
            last_statement = None });
         (True,
          { statements =
            [(CallStatement
                (Call ((PrefixVar (Name "print")), [(String "yaya")])))
              ];
            last_statement = None })
         ]) |}];
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
    (ForRange
       { name = "x"; start = 1; finish = 5; step = None;
         for_block =
         { statements =
           [(CallStatement
               (Call ((PrefixVar (Name "print")),
                  [(PrefixExpr (PrefixVar (Name "x")))])))
             ];
           last_statement = None }
         })
    (ForRange
       { name = "y"; start = 1; finish = 10; step = (Some 2);
         for_block =
         { statements =
           [(CallStatement
               (Call ((PrefixVar (Name "print")),
                  [(PrefixExpr (PrefixVar (Name "y")))])))
             ];
           last_statement = None }
         })
    (ForNames (["idx"; "value"],
       [(PrefixExpr
           (PrefixCall
              (Call ((PrefixVar (Name "ipairs")),
                 [(PrefixExpr (PrefixVar (Name "mylist")))]))))
         ],
       { statements =
         [(CallStatement
             (Call ((PrefixVar (Name "print")),
                [(PrefixExpr (PrefixVar (Name "idx")));
                  (PrefixExpr (PrefixVar (Name "value")))]
                )))
           ];
         last_statement = None }
       )) |}];
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
        [(CallStatement (Call ((PrefixVar (Name "print")), [(String "hi")])))];
        last_statement = None }}
    FunctionStatement {
      function_name =
      { Ast.FuncName.base = "with"; keys = ["args"]; table_method = None };
      function_parameters = { args = ["a"; "b"; "c"]; varargs = false };
      function_block =
      { statements =
        [(CallStatement
            (Call ((PrefixVar (Name "print")),
               [(PrefixExpr (PrefixVar (Name "b")));
                 (PrefixExpr (PrefixVar (Name "c")));
                 (PrefixExpr (PrefixVar (Name "a")))]
               )))
          ];
        last_statement = None }} |}];
  ()
;;

(*let%expect_test "locals" =*)
(*  Fmt.pr "1: %a" Ast.pp_expr (Parse.parse "local x = 5");*)
(*  [%expect {| |}];*)
(*  ()*)
(*;;*)
