Parsing Expression:
  $ lua_ast --expression --directory expressions/
  
  ===== expressions/maths.lua =====
  -- Integer Maths
  5 + 6 * 7
  (Add (5, (Mul (6, 7))))
  
  (5 + 5)
  (Add (5, 5))
  
  5 * 5
  (Mul (5, 5))
  
  1 * 2 ^ 3 + 4
  (Add ((Mul (1, (Exponent (2, 3)))), 4))
  
  (1 + 2) * 3
  (Mul ((Add (1, 2)), 3))
  
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
  (Name "hello")
  
  hello_world
  (Name "hello_world")
  
  HelloWorld
  (Name "HelloWorld")
  
  _Underscored
  (Name "_Underscored")
  
  _Numbers1234
  (Name "_Numbers1234")
  
  
  ===== expressions/strings.lua =====
  "hello" .. "middle" .. "world"
  (Concat ((String "hello"), (Concat ((String "middle"), (String "world")))))
  
  ("hello" .. "middle") .. "world"
  (Concat ((Concat ((String "hello"), (String "middle"))), (String "world")))
  
  
  ===== expressions/functions.lua =====
  function(a) return a end
  (Function
     { parameters = { args = ["a"]; varargs = false };
       block =
       { statements = []; last_statement = (Some (Return [(Name "a")])) } })
  
  function(a) return a + a end
  (Function
     { parameters = { args = ["a"]; varargs = false };
       block =
       { statements = [];
         last_statement = (Some (Return [(Add ((Name "a"), (Name "a")))])) }
       })
  
  function(a, b) return a * b end
  (Function
     { parameters = { args = ["a"; "b"]; varargs = false };
       block =
       { statements = [];
         last_statement = (Some (Return [(Mul ((Name "a"), (Name "b")))])) }
       })
  
  function(a, ...) return a * #{...} end
  (Function
     { parameters = { args = ["a"]; varargs = true };
       block =
       { statements = [];
         last_statement =
         (Some (Return [(Mul ((Name "a"), (Len (Table [(None, VarArgs)]))))]))
         }
       })
  
  
  ===== expressions/comments.lua =====
  -- hello world
  
  ===== expressions/call.lua =====
  -- stylua: ignore start
  -- Regular
  f()
  (CallExpr (Call ((Name "f"), [])))
  
  f("hello")
  (CallExpr (Call ((Name "f"), [(String "hello")])))
  
  f "hello"
  (CallExpr (Call ((Name "f"), [(String "hello")])))
  
  f({})
  (CallExpr (Call ((Name "f"), [(Table [])])))
  
  f {}
  (CallExpr (Call ((Name "f"), [(Table [])])))
  
  f { table = true }
  (CallExpr (Call ((Name "f"), [(Table [((Some (Name "table")), True)])])))
  
  
  -- Tables
  t:name()
  (CallExpr (Self ((Name "t"), "name", [])))
  
  t:name("hello")
  (CallExpr (Self ((Name "t"), "name", [(String "hello")])))
  
  t:name "hello"
  (CallExpr (Self ((Name "t"), "name", [(String "hello")])))
  
  t:name({})
  (CallExpr (Self ((Name "t"), "name", [(Table [])])))
  
  t:name {}
  (CallExpr (Self ((Name "t"), "name", [(Table [])])))
  
  t:name { table = true }
  (CallExpr
     (Self ((Name "t"), "name", [(Table [((Some (Name "table")), True)])])))
  
  -- stylua: ignore end
  
  ===== expressions/unary.lua =====
  not true
  (Not True)
  
  -5
  -5
  
  -hello
  (Neg (Name "hello"))
  
  #{1, 2}
  (Len (Table [(None, 1); (None, 2)]))
  
  #({1, 2})
  (Len (Table [(None, 1); (None, 2)]))
  
  -yes + 7
  (Add ((Neg (Name "yes")), 7))
  
  #"hello"
  (Len (String "hello"))
  
  #("hello")
  (Len (String "hello"))
  
