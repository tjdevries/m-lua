Parsing Expression:
  $ lua_ast --expression --directory expressions/
  
  ===== expressions/maths.lua =====
  -- Integer Maths
  5 + 6 * 7
  (Add (5, (Mul (6, 7))))
  
  (5 + 5)
  (PrefixExpr (PrefixParens (Add (5, 5))))
  
  5 * 5
  (Mul (5, 5))
  
  1 * 2 ^ 3 + 4
  (Add ((Mul (1, (Exponent (2, 3)))), 4))
  
  (1 + 2) * 3
  (Mul ((PrefixExpr (PrefixParens (Add (1, 2)))), 3))
  
  13 % 2
  (Mod (13, 2))
  
  
  -- Floats
  3.14 + 1234
  (Add (3.14, 1234))
  
  3.14 / 1.07
  (Div (3.14, 1.07))
  
  
  ===== expressions/identifiers.lua =====
  true
  True
  
  false
  False
  
  nil
  Nil
  
  hello
  (PrefixExpr (PrefixVar (Name "hello")))
  
  hello_world
  (PrefixExpr (PrefixVar (Name "hello_world")))
  
  HelloWorld
  (PrefixExpr (PrefixVar (Name "HelloWorld")))
  
  _Underscored
  (PrefixExpr (PrefixVar (Name "_Underscored")))
  
  _Numbers1234
  (PrefixExpr (PrefixVar (Name "_Numbers1234")))
  
  
  ===== expressions/strings.lua =====
  "hello" .. "middle" .. "world"
  (Concat ((String "hello"), (Concat ((String "middle"), (String "world")))))
  
  ("hello" .. "middle") .. "world"
  (Concat (
     (PrefixExpr (PrefixParens (Concat ((String "hello"), (String "middle"))))),
     (String "world")))
  
  
  ===== expressions/functions.lua =====
  function(a) return a end
  (Function
     { parameters = { args = ["a"]; varargs = false };
       block =
       { statements = [];
         last_statement = (Some (Return [(PrefixExpr (PrefixVar (Name "a")))]))
         }
       })
  
  function(a) return a + a end
  (Function
     { parameters = { args = ["a"]; varargs = false };
       block =
       { statements = [];
         last_statement =
         (Some (Return
                  [(Add ((PrefixExpr (PrefixVar (Name "a"))),
                      (PrefixExpr (PrefixVar (Name "a")))))
                    ]))
         }
       })
  
  function(a, b) return a * b end
  (Function
     { parameters = { args = ["a"; "b"]; varargs = false };
       block =
       { statements = [];
         last_statement =
         (Some (Return
                  [(Mul ((PrefixExpr (PrefixVar (Name "a"))),
                      (PrefixExpr (PrefixVar (Name "b")))))
                    ]))
         }
       })
  
  function(a, ...) return a * #{...} end
  (Function
     { parameters = { args = ["a"]; varargs = true };
       block =
       { statements = [];
         last_statement =
         (Some (Return
                  [(Mul ((PrefixExpr (PrefixVar (Name "a"))),
                      (Len (Table [(None, VarArgs)]))))
                    ]))
         }
       })
  
  
  ===== expressions/comments.lua =====
  -- hello world
  
  ===== expressions/call.lua =====
  -- stylua: ignore start
  -- Regular
  f()
  (PrefixExpr (PrefixCall (Call ((PrefixVar (Name "f")), []))))
  
  f("hello")
  (PrefixExpr (PrefixCall (Call ((PrefixVar (Name "f")), [(String "hello")]))))
  
  f "hello"
  (PrefixExpr (PrefixCall (Call ((PrefixVar (Name "f")), [(String "hello")]))))
  
  f({})
  (PrefixExpr (PrefixCall (Call ((PrefixVar (Name "f")), [(Table [])]))))
  
  f {}
  (PrefixExpr (PrefixCall (Call ((PrefixVar (Name "f")), [(Table [])]))))
  
  f { table = true }
  (PrefixExpr
     (PrefixCall
        (Call ((PrefixVar (Name "f")),
           [(Table [((Some (String "table")), True)])]))))
  
  
  -- Tables
  t:name()
  (PrefixExpr (PrefixCall (Self ((PrefixVar (Name "t")), "name", []))))
  
  t:name("hello")
  (PrefixExpr
     (PrefixCall (Self ((PrefixVar (Name "t")), "name", [(String "hello")]))))
  
  t:name "hello"
  (PrefixExpr
     (PrefixCall (Self ((PrefixVar (Name "t")), "name", [(String "hello")]))))
  
  t:name({})
  (PrefixExpr
     (PrefixCall (Self ((PrefixVar (Name "t")), "name", [(Table [])]))))
  
  t:name {}
  (PrefixExpr
     (PrefixCall (Self ((PrefixVar (Name "t")), "name", [(Table [])]))))
  
  t:name { table = true }
  (PrefixExpr
     (PrefixCall
        (Self ((PrefixVar (Name "t")), "name",
           [(Table [((Some (String "table")), True)])]))))
  
  -- stylua: ignore end
  
  ===== expressions/unary.lua =====
  not true
  (Not True)
  
  -5
  -5
  
  -hello
  (Neg (PrefixExpr (PrefixVar (Name "hello"))))
  
  #{1, 2}
  (Len (Table [(None, 1); (None, 2)]))
  
  #({1, 2})
  (Len (PrefixExpr (PrefixParens (Table [(None, 1); (None, 2)]))))
  
  -yes + 7
  (Add ((Neg (PrefixExpr (PrefixVar (Name "yes")))), 7))
  
  #"hello"
  (Len (String "hello"))
  
  #("hello")
  (Len (PrefixExpr (PrefixParens (String "hello"))))
  
