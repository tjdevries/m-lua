Simple Test:
  $ lua_ast --program --directory ./statements/
  
  ===== ./statements/conditionals.lua =====
  [(If
      [(True,
        { statements =
          [(CallStatement
              (Call ((PrefixVar (Name "print")), [(String "inside true"); Nil]
                 )))
            ];
          last_statement = None })
        ]);
    (If
       [(False,
         { statements =
           [(CallStatement
               (Call ((PrefixVar (Name "print")), [(String "should not print")]
                  )))
             ];
           last_statement = None });
         (False, { statements = []; last_statement = None });
         (False, { statements = []; last_statement = None });
         (True,
          { statements =
            [(CallStatement
                (Call ((PrefixVar (Name "print")), [(String "in else block")])))
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
       (Call ((PrefixVar (Name "print")),
          [(String "inside:"); (PrefixExpr (PrefixVar (Name "inside")))])))
    ]
  
  ===== ./statements/print.lua =====
  [(CallStatement
      (Call ((PrefixVar (Name "print")), [(String "h"); (String "hello world")]
         )));
    (CallStatement (Call ((PrefixVar (Name "print")), [(String "line 2")])));
    (CallStatement
       (Call ((PrefixVar (Name "print")), [(String "escaped \\\" ")])));
    (CallStatement
       (Call ((PrefixVar (Name "print")), [(String "escaped \\' ")])));
    (CallStatement
       (Call ((PrefixVar (Name "print")),
          [(Add ((Mul (5, 5.01)), (Mul (37, 2))))])));
    (CallStatement (Call ((PrefixVar (Name "print")), [(Table [(None, 1)])])));
    (CallStatement
       (Call ((PrefixVar (Name "print")),
          [(Table [(None, 1); (None, 2); (None, 3)])])));
    (CallStatement
       (Call ((PrefixVar (Name "print")),
          [(PrefixExpr (PrefixCall (Call ((PrefixVar (Name "tostring")), [1]))))
            ]
          )));
    (CallStatement
       (Call ((PrefixVar (Name "print")),
          [(PrefixExpr
              (PrefixCall (Call ((PrefixVar (Name "tostring")), [(Table [])]))))
            ]
          )))
    ]
  
  ===== ./statements/tables.lua =====
  [(LocalBinding (["t"],
      [(Table
          [(None, (String "a")); (None, (String "b")); (None, (String "c"))])
        ]
      ));
    (CallStatement
       (Call ((PrefixVar (Name "print")),
          [(String "index t[1] -> 'a'");
            (PrefixExpr (PrefixVar (Index ((PrefixVar (Name "t")), 1))))]
          )));
    (LocalBinding (["index"], [True]));
    (LocalBinding (["other_index"], [False]));
    (LocalBinding (["func_index"],
       [(Function
           { parameters = { args = []; varargs = false };
             block = { statements = []; last_statement = None } })
         ]
       ));
    (LocalBinding (["string_table"],
       [(Table
           [((Some (String "based")), True);
             ((Some (String "ocaml")), (String "cool"));
             ((Some (String "a string")), (String "wow"));
             ((Some (PrefixExpr (PrefixVar (Name "index")))),
              (String "berry true"));
             ((Some (PrefixExpr (PrefixVar (Name "other_index")))),
              (String "INCREDIBLE"));
             ((Some (PrefixExpr (PrefixVar (Name "func_index")))),
              (String "yes, functions can be keys"))
             ])
         ]
       ));
    (CallStatement
       (Call ((PrefixVar (Name "print")),
          [(String "based =");
            (PrefixExpr
               (PrefixVar (Dot ((PrefixVar (Name "string_table")), "based"))))
            ]
          )));
    (CallStatement
       (Call ((PrefixVar (Name "print")),
          [(String "ocaml is");
            (PrefixExpr
               (PrefixVar
                  (Index ((PrefixVar (Name "string_table")), (String "ocaml")))))
            ]
          )));
    (CallStatement
       (Call ((PrefixVar (Name "print")),
          [(String "ocaml is also");
            (PrefixExpr
               (PrefixVar (Dot ((PrefixVar (Name "string_table")), "ocaml"))))
            ]
          )));
    (CallStatement
       (Call ((PrefixVar (Name "print")),
          [(String "this index is");
            (PrefixExpr
               (PrefixVar
                  (Index ((PrefixVar (Name "string_table")),
                     (String "a string")))))
            ]
          )));
    (CallStatement
       (Call ((PrefixVar (Name "print")),
          [(String "this index is also");
            (PrefixExpr
               (PrefixVar
                  (Index ((PrefixVar (Name "string_table")),
                     (PrefixExpr (PrefixVar (Name "index")))))))
            ]
          )));
    (CallStatement
       (Call ((PrefixVar (Name "print")),
          [(String "other index is");
            (PrefixExpr
               (PrefixVar
                  (Index ((PrefixVar (Name "string_table")),
                     (PrefixExpr (PrefixVar (Name "other_index")))))))
            ]
          )));
    (CallStatement
       (Call ((PrefixVar (Name "print")),
          [(String "with a literal");
            (PrefixExpr
               (PrefixVar (Index ((PrefixVar (Name "string_table")), False))))
            ]
          )));
    (CallStatement
       (Call ((PrefixVar (Name "print")),
          [(String "func_index is");
            (PrefixExpr
               (PrefixVar
                  (Index ((PrefixVar (Name "string_table")),
                     (PrefixExpr (PrefixVar (Name "func_index")))))))
            ]
          )))
    ]
  
  ===== ./statements/scopes.lua =====
  [(CallStatement
      (Call ((PrefixVar (Name "print")),
         [(PrefixExpr (PrefixVar (Name "not_defined")))])))
    ]
  
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
                                    (Some (Return
                                             [(PrefixExpr
                                                 (PrefixVar (Name "x")))
                                               ]))
                                    })
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
       (Call ((PrefixVar (Name "print")),
          [(PrefixExpr (PrefixCall (Call ((PrefixVar (Name "myfunc")), []))));
            (String "|"); (PrefixExpr (PrefixVar (Name "x")))]
          )));
    (LocalBinding (["shared"], [5]));
    (LocalBinding (["shared_func"],
       [(Function
           { parameters = { args = ["shared"; "optional"]; varargs = false };
             block =
             { statements = [];
               last_statement =
               (Some (Return [(PrefixExpr (PrefixVar (Name "shared")))])) }
             })
         ]
       ));
    (CallStatement
       (Call ((PrefixVar (Name "print")),
          [(PrefixExpr
              (PrefixCall (Call ((PrefixVar (Name "shared_func")), [False]))))
            ]
          )));
    (LocalBinding (["outside"], [True]));
    (LocalBinding (["outside_func"],
       [(Function
           { parameters = { args = ["outside"]; varargs = false };
             block =
             { statements = [];
               last_statement =
               (Some (Return [(PrefixExpr (PrefixVar (Name "outside")))])) }
             })
         ]
       ));
    (CallStatement
       (Call ((PrefixVar (Name "print")),
          [(PrefixExpr
              (PrefixCall (Call ((PrefixVar (Name "outside_func")), []))))
            ]
          )));
    (LocalBinding (["additional_func"],
       [(Function
           { parameters = { args = ["x"]; varargs = false };
             block =
             { statements = [];
               last_statement =
               (Some (Return [(PrefixExpr (PrefixVar (Name "x")))])) }
             })
         ]
       ));
    (CallStatement
       (Call ((PrefixVar (Name "print")),
          [(PrefixExpr
              (PrefixCall
                 (Call ((PrefixVar (Name "additional_func")), [10; 11; 12]))))
            ]
          )))
    ]
  
  ===== ./statements/comments.lua =====
  [(LocalBinding (["x"], [1])); (LocalBinding (["y"], [True]));
    (LocalBinding (["another"], [False]))]
  
  ===== ./statements/for-range-loops.lua =====
  [(CallStatement (Call ((PrefixVar (Name "print")), [(String "for 1, 10")])));
    (ForRange
       { name = "i"; start = 1; finish = 10; step = None;
         for_block =
         { statements =
           [(CallStatement
               (Call ((PrefixVar (Name "print")),
                  [(PrefixExpr (PrefixVar (Name "i")))])))
             ];
           last_statement = None }
         });
    (CallStatement
       (Call ((PrefixVar (Name "print")), [(String "for 1, 10, 1.5")])));
    (ForRange
       { name = "i"; start = 1; finish = 10; step = (Some 1.5);
         for_block =
         { statements =
           [(CallStatement
               (Call ((PrefixVar (Name "print")),
                  [(PrefixExpr (PrefixVar (Name "i")))])))
             ];
           last_statement = None }
         });
    (CallStatement
       (Call ((PrefixVar (Name "print")), [(String "for 10, 1, -1")])));
    (ForRange
       { name = "i"; start = 10; finish = 1; step = (Some -1);
         for_block =
         { statements =
           [(CallStatement
               (Call ((PrefixVar (Name "print")),
                  [(PrefixExpr (PrefixVar (Name "i")))])))
             ];
           last_statement = None }
         });
    (CallStatement (Call ((PrefixVar (Name "print")), [(String "break")])));
    (LocalBinding (["sum"], [0]));
    (ForRange
       { name = "i"; start = 1; finish = 100; step = (Some 1);
         for_block =
         { statements =
           [(Binding ([(Name "sum")],
               [(Add ((PrefixExpr (PrefixVar (Name "sum"))),
                   (PrefixExpr (PrefixVar (Name "i")))))
                 ]
               ));
             (If
                [((GTE ((PrefixExpr (PrefixVar (Name "i"))), 3)),
                  { statements = []; last_statement = (Some Break) })])
             ];
           last_statement = None }
         });
    (CallStatement
       (Call ((PrefixVar (Name "print")),
          [(String "broken sum:"); (PrefixExpr (PrefixVar (Name "sum")))])));
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
                           [((EQ ((PrefixExpr (PrefixVar (Name "i"))), 5)),
                             { statements = [];
                               last_statement =
                               (Some (Return
                                        [(PrefixExpr (PrefixVar (Name "i")))]))
                               })
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
       (Call ((PrefixVar (Name "print")),
          [(String "should be 5");
            (PrefixExpr (PrefixCall (Call ((PrefixVar (Name "x")), []))))]
          )));
    (CallStatement
       (Call ((PrefixVar (Name "print")), [(String "loop with exprs")])));
    (LocalBinding (["my_func"],
       [(Function
           { parameters = { args = ["v"]; varargs = false };
             block =
             { statements = [];
               last_statement =
               (Some (Return [(Add ((PrefixExpr (PrefixVar (Name "v"))), 1))]))
               }
             })
         ]
       ));
    (ForRange
       { name = "i";
         start =
         (PrefixExpr (PrefixCall (Call ((PrefixVar (Name "my_func")), [1]))));
         finish = 4; step = None;
         for_block =
         { statements =
           [(CallStatement
               (Call ((PrefixVar (Name "print")),
                  [(PrefixExpr (PrefixVar (Name "i")))])))
             ];
           last_statement = None }
         })
    ]
  
  ===== ./statements/locals.lua =====
  [(LocalBinding (["x"], [(String "hi LLL - 1 is very based")]));
    (CallStatement
       (Call ((PrefixVar (Name "print")), [(PrefixExpr (PrefixVar (Name "x")))]
          )));
    (LocalBinding (["y"], [10])); (LocalBinding (["z"], [15]));
    (CallStatement
       (Call ((PrefixVar (Name "print")),
          [(String "first"); (PrefixExpr (PrefixVar (Name "y"))); (String "+");
            (PrefixExpr (PrefixVar (Name "z"))); (String "=");
            (Add ((PrefixExpr (PrefixVar (Name "y"))),
               (PrefixExpr (PrefixVar (Name "z")))))
            ]
          )));
    (Do
       { statements =
         [(LocalBinding (["abc"], [10]));
           (CallStatement
              (Call ((PrefixVar (Name "print")),
                 [(String "in a do block");
                   (PrefixExpr (PrefixVar (Name "abc"))); (String "(");
                   (PrefixExpr (PrefixVar (Name "y")));
                   (PrefixExpr (PrefixVar (Name "z"))); (String ")")]
                 )))
           ];
         last_statement = None })
    ]
  
  ===== ./statements/for-expr-loops.lua =====
  [(CallStatement
      (Call ((PrefixVar (Name "print")), [(String "starting expr loops")])));
    (LocalBinding (["t"],
       [(Table
           [((Some -1), (String "skipped"));
             ((Some 0), (String "KEKW IMAGINE STARTING AT 0 NOT IN C"));
             (None, (String "a")); (None, (String "b")); (None, (String "c"));
             (None, True); (None, False); ((Some 6), (String "wow"));
             ((Some 7), (Table []))])
         ]
       ));
    (ForNames (["i"; "v"],
       [(PrefixExpr
           (PrefixCall
              (Call ((PrefixVar (Name "ipairs")),
                 [(PrefixExpr (PrefixVar (Name "t")))]))))
         ],
       { statements =
         [(CallStatement
             (Call ((PrefixVar (Name "print")),
                [(String "expr-loop:"); (PrefixExpr (PrefixVar (Name "i")));
                  (PrefixExpr (PrefixVar (Name "v")))]
                )))
           ];
         last_statement = None }
       ));
    (CallStatement
       (Call ((PrefixVar (Name "print")), [(String "ending expr loop")])));
    (CallStatement
       (Call ((PrefixVar (Name "print")), [(String "starting pairs loop")])));
    (LocalBinding (["map"],
       [(Table
           [(None, (String "first")); (None, (String "second"));
             ((Some (String "key")), (String "value"));
             ((Some (String "ocaml")), (String "based"));
             ((Some (String "teej_dv")), (String "like & subscribe"))])
         ]
       ));
    (ForNames (["k"; "v"],
       [(PrefixExpr
           (PrefixCall
              (Call ((PrefixVar (Name "pairs")),
                 [(PrefixExpr (PrefixVar (Name "map")))]))))
         ],
       { statements =
         [(CallStatement
             (Call ((PrefixVar (Name "print")),
                [(String "pairs-loop:"); (PrefixExpr (PrefixVar (Name "k")));
                  (PrefixExpr (PrefixVar (Name "v")))]
                )))
           ];
         last_statement = None }
       ));
    (CallStatement
       (Call ((PrefixVar (Name "print")), [(String "ending pairs loop")])));
    (CallStatement
       (Call ((PrefixVar (Name "print")), [(String "custom iterator")])));
    (LocalBinding (["my_func"],
       [(Function
           { parameters = { args = ["_"; "var"]; varargs = false };
             block =
             { statements =
               [(Binding ([(Name "var")],
                   [(Add ((PrefixExpr (PrefixVar (Name "var"))), 1))]));
                 (If
                    [((GT ((PrefixExpr (PrefixVar (Name "var"))), 5)),
                      { statements = []; last_statement = (Some (Return [Nil]))
                        });
                      ((EQ ((Mod ((PrefixExpr (PrefixVar (Name "var"))), 2)), 0
                          )),
                       { statements = [];
                         last_statement =
                         (Some (Return
                                  [(PrefixExpr (PrefixVar (Name "var")));
                                    (String "even")]))
                         });
                      (True,
                       { statements = [];
                         last_statement =
                         (Some (Return
                                  [(PrefixExpr (PrefixVar (Name "var")));
                                    (String "odd")]))
                         })
                      ])
                 ];
               last_statement = None }
             })
         ]
       ));
    (ForNames (["i"; "even_odd"],
       [(PrefixExpr (PrefixVar (Name "my_func"))); Nil; 0],
       { statements =
         [(CallStatement
             (Call ((PrefixVar (Name "print")),
                [(PrefixExpr (PrefixVar (Name "i")));
                  (PrefixExpr (PrefixVar (Name "even_odd")))]
                )))
           ];
         last_statement = None }
       ));
    (CallStatement
       (Call ((PrefixVar (Name "print")), [(String "end custom iterator")])))
    ]

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
  <table>
  <table>
  1
  <table>
  { Environment.locals = <lookup tbl>; parent = None }
  
  ===== ./statements/tables.lua =====
  index t[1] -> 'a' a
  based = true
  ocaml is cool
  ocaml is also cool
  this index is wow
  this index is also berry true
  other index is INCREDIBLE
  with a literal INCREDIBLE
  func_index is yes, functions can be keys
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
  expr-loop: 7 <table>
  ending expr loop
  starting pairs loop
  pairs-loop: 1 first
  pairs-loop: 2 second
  pairs-loop: ocaml based
  pairs-loop: key value
  pairs-loop: teej_dv like & subscribe
  ending pairs loop
  custom iterator
  1 odd
  2 even
  3 odd
  4 even
  5 odd
  end custom iterator
  { Environment.locals = <lookup tbl>; parent = None }
