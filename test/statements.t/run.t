Simple Test:
  $ lua_ast --program --directory ./statements/
  
  ===== ./statements/conditionals.lua =====
  [(If
      [(True,
        { statements =
          [(CallStatement
              (Call ((Name "print"), [(String "inside true"); Nil])))
            ];
          last_statement = None })
        ]);
    (If
       [(False,
         { statements =
           [(CallStatement
               (Call ((Name "print"), [(String "should not print")])))
             ];
           last_statement = None });
         (False, { statements = []; last_statement = None });
         (False, { statements = []; last_statement = None });
         (True,
          { statements =
            [(CallStatement (Call ((Name "print"), [(String "in else block")])))
              ];
            last_statement = None })
         ]);
    (LocalBinding (["inside"], [False]));
    (If
       [(True,
         { statements = [(Binding ([(Name "inside")], [True]))];
           last_statement = None })
         ]);
    (CallStatement
       (Call ((Name "print"), [(String "inside:"); (Name "inside")])))
    ]
  
  ===== ./statements/print.lua =====
  [(CallStatement
      (Call ((Name "print"), [(String "h"); (String "hello world")])));
    (CallStatement (Call ((Name "print"), [(String "line 2")])));
    (CallStatement (Call ((Name "print"), [(String "escaped \\\" ")])));
    (CallStatement (Call ((Name "print"), [(String "escaped \\' ")])));
    (CallStatement
       (Call ((Name "print"), [(Add ((Mul (5, 5.01)), (Mul (37, 2))))])));
    (CallStatement (Call ((Name "print"), [(Table [(None, 1)])])));
    (CallStatement
       (Call ((Name "print"), [(Table [(None, 1); (None, 2); (None, 3)])])));
    (CallStatement
       (Call ((Name "print"), [(CallExpr (Call ((Name "tostring"), [1])))])));
    (CallStatement
       (Call ((Name "print"),
          [(CallExpr (Call ((Name "tostring"), [(Table [])])))])))
    ]
  
  ===== ./statements/tables.lua =====
  [(LocalBinding (["t"],
      [(Table
          [(None, (String "a")); (None, (String "b")); (None, (String "c"))])
        ]
      ));
    (CallStatement
       (Call ((Name "print"),
          [(String "index t[1] -> 'a'"); (Index ((Name "t"), 1))])));
    (LocalBinding (["string_table"],
       [(Table
           [((Some (Name "based")), True);
             ((Some (Name "ocaml")), (String "cool"))])
         ]
       ));
    (CallStatement
       (Call ((Name "print"), [(Dot ((Name "string_table"), "based"))])))
    ]
  
  ===== ./statements/scopes.lua =====
  [(CallStatement (Call ((Name "print"), [(Name "not_defined")])))]
  
  ===== ./statements/functions.lua =====
  [(LocalBinding (["x"], [5]));
    (LocalBinding (["myfunc"],
       [(Function
           { parameters = { args = []; varargs = false };
             block =
             { statements =
               [(Do
                   { statements = [(LocalBinding (["x"], [(String "plz no")]))];
                     last_statement = None });
                 (Binding ([(Name "x")], [6]));
                 (Do
                    { statements =
                      [(Do
                          { statements =
                            [(If
                                [(True,
                                  { statements = [];
                                    last_statement =
                                    (Some (Return [(Name "x")])) })
                                  ])
                              ];
                            last_statement = None })
                        ];
                      last_statement = None })
                 ];
               last_statement = None }
             })
         ]
       ));
    (CallStatement
       (Call ((Name "print"),
          [(CallExpr (Call ((Name "myfunc"), []))); (String "|"); (Name "x")])));
    (LocalBinding (["shared"], [5]));
    (LocalBinding (["shared_func"],
       [(Function
           { parameters = { args = ["shared"; "optional"]; varargs = false };
             block =
             { statements = [];
               last_statement = (Some (Return [(Name "shared")])) }
             })
         ]
       ));
    (CallStatement
       (Call ((Name "print"),
          [(CallExpr (Call ((Name "shared_func"), [False])))])));
    (LocalBinding (["outside"], [True]));
    (LocalBinding (["outside_func"],
       [(Function
           { parameters = { args = ["outside"]; varargs = false };
             block =
             { statements = [];
               last_statement = (Some (Return [(Name "outside")])) }
             })
         ]
       ));
    (CallStatement
       (Call ((Name "print"), [(CallExpr (Call ((Name "outside_func"), [])))])));
    (LocalBinding (["additional_func"],
       [(Function
           { parameters = { args = ["x"]; varargs = false };
             block =
             { statements = []; last_statement = (Some (Return [(Name "x")])) }
             })
         ]
       ));
    (CallStatement
       (Call ((Name "print"),
          [(CallExpr (Call ((Name "additional_func"), [10; 11; 12])))])))
    ]
  
  ===== ./statements/comments.lua =====
  [(LocalBinding (["x"], [1])); (LocalBinding (["y"], [True]));
    (LocalBinding (["another"], [False]))]
  
  ===== ./statements/for-range-loops.lua =====
  [(CallStatement (Call ((Name "print"), [(String "for 1, 10")])));
    (ForRange
       { name = "i"; start = 1; finish = 10; step = None;
         for_block =
         { statements = [(CallStatement (Call ((Name "print"), [(Name "i")])))];
           last_statement = None }
         });
    (CallStatement (Call ((Name "print"), [(String "for 1, 10, 1.5")])));
    (ForRange
       { name = "i"; start = 1; finish = 10; step = (Some 1.5);
         for_block =
         { statements = [(CallStatement (Call ((Name "print"), [(Name "i")])))];
           last_statement = None }
         });
    (CallStatement (Call ((Name "print"), [(String "for 10, 1, -1")])));
    (ForRange
       { name = "i"; start = 10; finish = 1; step = (Some -1);
         for_block =
         { statements = [(CallStatement (Call ((Name "print"), [(Name "i")])))];
           last_statement = None }
         });
    (CallStatement (Call ((Name "print"), [(String "break")])));
    (LocalBinding (["sum"], [0]));
    (ForRange
       { name = "i"; start = 1; finish = 100; step = (Some 1);
         for_block =
         { statements =
           [(Binding ([(Name "sum")], [(Add ((Name "sum"), (Name "i")))]));
             (If
                [((GTE ((Name "i"), 3)),
                  { statements = []; last_statement = (Some Break) })])
             ];
           last_statement = None }
         });
    (CallStatement
       (Call ((Name "print"), [(String "broken sum:"); (Name "sum")])));
    (LocalBinding (["x"],
       [(Function
           { parameters = { args = []; varargs = false };
             block =
             { statements =
               [(ForRange
                   { name = "i"; start = 1; finish = 10; step = None;
                     for_block =
                     { statements =
                       [(If
                           [((EQ ((Name "i"), 5)),
                             { statements = [];
                               last_statement = (Some (Return [(Name "i")])) })
                             ])
                         ];
                       last_statement = None }
                     })
                 ];
               last_statement = (Some (Return [(String "<this is an error>")]))
               }
             })
         ]
       ));
    (CallStatement
       (Call ((Name "print"),
          [(String "should be 5"); (CallExpr (Call ((Name "x"), [])))])));
    (CallStatement (Call ((Name "print"), [(String "loop with exprs")])));
    (LocalBinding (["my_func"],
       [(Function
           { parameters = { args = ["v"]; varargs = false };
             block =
             { statements = [];
               last_statement = (Some (Return [(Add ((Name "v"), 1))])) }
             })
         ]
       ));
    (ForRange
       { name = "i"; start = (CallExpr (Call ((Name "my_func"), [1])));
         finish = 4; step = None;
         for_block =
         { statements = [(CallStatement (Call ((Name "print"), [(Name "i")])))];
           last_statement = None }
         })
    ]
  
  ===== ./statements/locals.lua =====
  [(LocalBinding (["x"], [(String "hi LLL - 1 is very based")]));
    (CallStatement (Call ((Name "print"), [(Name "x")])));
    (LocalBinding (["y"], [10])); (LocalBinding (["z"], [15]));
    (CallStatement
       (Call ((Name "print"),
          [(String "first"); (Name "y"); (String "+"); (Name "z");
            (String "="); (Add ((Name "y"), (Name "z")))]
          )));
    (Do
       { statements =
         [(LocalBinding (["abc"], [10]));
           (CallStatement
              (Call ((Name "print"),
                 [(String "in a do block"); (Name "abc"); (String "(");
                   (Name "y"); (Name "z"); (String ")")]
                 )))
           ];
         last_statement = None })
    ]
  
  ===== ./statements/for-expr-loops.lua =====
  [(CallStatement (Call ((Name "print"), [(String "starting expr loops")])));
    (LocalBinding (["t"],
       [(Table
           [((Some -1), (String "skipped"));
             ((Some 0), (String "KEKW IMAGINE STARTING AT 0 NOT IN C"));
             (None, (String "a")); (None, (String "b")); (None, (String "c"));
             (None, True); (None, False); ((Some 6), (String "wow"));
             ((Some 7), (Table []))])
         ]
       ));
    (ForNames (["i"; "v"], [(CallExpr (Call ((Name "ipairs"), [(Name "t")])))],
       { statements =
         [(CallStatement
             (Call ((Name "print"),
                [(String "expr-loop:"); (Name "i"); (Name "v")])))
           ];
         last_statement = None }
       ));
    (CallStatement (Call ((Name "print"), [(String "ending expr loop")])))]

  $ lua_eval --program --directory ./statements/
  
  ===== ./statements/conditionals.lua =====
  inside true nil 
  in else block 
  inside: true 
  { Environment.locals = <lookup tbl>; parent = None }
  
  ===== ./statements/print.lua =====
  h hello world 
  line 2 
  escaped \"  
  escaped \'  
  99.05 
  <table: 6> 
  <table: 7> 
  1 
  <table: 8> 
  { Environment.locals = <lookup tbl>; parent = None }
  
  ===== ./statements/tables.lua =====
  index t[1] -> 'a' a 
  nil 
  { Environment.locals = <lookup tbl>; parent = None }
  
  ===== ./statements/scopes.lua =====
  nil 
  { Environment.locals = <lookup tbl>; parent = None }
  
  ===== ./statements/functions.lua =====
  6 | 6 
  false 
  nil 
  10 
  { Environment.locals = <lookup tbl>; parent = None }
  
  ===== ./statements/comments.lua =====
  { Environment.locals = <lookup tbl>; parent = None }
  
  ===== ./statements/for-range-loops.lua =====
  for 1, 10 
  1 
  2 
  3 
  4 
  5 
  6 
  7 
  8 
  9 
  10 
  for 1, 10, 1.5 
  1 
  2.5 
  4 
  5.5 
  7 
  8.5 
  10 
  for 10, 1, -1 
  10 
  9 
  8 
  7 
  6 
  5 
  4 
  3 
  2 
  1 
  break 
  broken sum: 6 
  should be 5 5 
  loop with exprs 
  2 
  3 
  4 
  { Environment.locals = <lookup tbl>; parent = None }
  
  ===== ./statements/locals.lua =====
  hi LLL - 1 is very based 
  first 10 + 15 = 25 
  in a do block 10 ( 10 15 ) 
  { Environment.locals = <lookup tbl>; parent = None }
  
  ===== ./statements/for-expr-loops.lua =====
  starting expr loops 
  expr-loop: 1 a 
  expr-loop: 2 b 
  expr-loop: 3 c 
  expr-loop: 4 true 
  expr-loop: 5 false 
  expr-loop: 6 wow 
  expr-loop: 7 <table: 17> 
  ending expr loop 
  { Environment.locals = <lookup tbl>; parent = None }
