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
  ===== ./statements/scopes.lua =====
  [(CallStatement (Call ((Name "print"), [(Name "not_defined")])))]
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

  $ lua_eval --program --directory ./statements/
  ===== ./statements/conditionals.lua =====
  inside true nil 
  in else block 
  inside: true 
  { Environment.locals = <lookup tbl>;
    parent =
    (Some { Environment.locals = <lookup tbl>;
            parent =
            (Some { Environment.locals = <lookup tbl>;
                    parent =
                    (Some { Environment.locals = <lookup tbl>;
                            parent =
                            (Some { Environment.locals = <lookup tbl>;
                                    parent = None })
                            })
                    })
            })
    }
  ===== ./statements/print.lua =====
  h hello world 
  line 2 
  escaped \"  
  escaped \'  
  99.05 
  <table: 3> 
  <table: 4> 
  1 
  <table: 5> 
  { Environment.locals = <lookup tbl>; parent = None }
  ===== ./statements/scopes.lua =====
  nil 
  { Environment.locals = <lookup tbl>; parent = None }
  ===== ./statements/locals.lua =====
  hi LLL - 1 is very based 
  first 10 + 15 = 25 
  in a do block 10 ( 10 15 ) 
  { Environment.locals = <lookup tbl>; parent = None }
