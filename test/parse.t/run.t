Simple Test:
  $ lua_ast --program --directory ./statements/
  ===== ./statements/conditionals.lua =====
  [If {
     conditions =
     [(True,
       { statements =
         [(CallStatement
             Call {prefix = (Name "print"); args = [(String "truthful")]})
           ];
         last_statement = None })
       ]};
    If {
      conditions =
      [(False,
        { statements =
          [(CallStatement
              Call {prefix = (Name "print");
                args = [(String "should not print")]})
            ];
          last_statement = None });
        (False, { statements = []; last_statement = None });
        (False, { statements = []; last_statement = None });
        (True,
         { statements =
           [(CallStatement
               Call {prefix = (Name "print"); args = [(String "in else block")]})
             ];
           last_statement = None })
        ]};
    LocalBinding {names = ["inside"]; exprs = [False]};
    If {
      conditions =
      [(True,
        { statements = [(Binding ([(Name "inside")], [True]))];
          last_statement = None })
        ]};
    (CallStatement
       Call {prefix = (Name "print");
         args = [(String "inside:"); (Name "inside")]})
    ]
  ===== ./statements/print.lua =====
  [(CallStatement
      Call {prefix = (Name "print");
        args = [(String "h"); (String "hello world")]});
    (CallStatement Call {prefix = (Name "print"); args = [(String "line 2")]});
    (CallStatement
       Call {prefix = (Name "print"); args = [(String "escaped \\\" ")]});
    (CallStatement
       Call {prefix = (Name "print"); args = [(String "escaped \\' ")]});
    (CallStatement
       Call {prefix = (Name "print");
         args = [(Add ((Mul (5, 5.01)), (Mul (37, 2))))]});
    (CallStatement
       Call {prefix = (Name "print");
         args = [(Table [{ key = None; value = 1 }])]});
    (CallStatement
       Call {prefix = (Name "print");
         args =
         [(Table
             [{ key = None; value = 1 }; { key = None; value = 2 };
               { key = None; value = 3 }])
           ]});
    (CallStatement
       Call {prefix = (Name "print");
         args = [(CallExpr Call {prefix = (Name "tostring"); args = [1]})]});
    (CallStatement
       Call {prefix = (Name "print");
         args =
         [(CallExpr Call {prefix = (Name "tostring"); args = [(Table [])]})]})
    ]
  ===== ./statements/scopes.lua =====
  [(CallStatement Call {prefix = (Name "print"); args = [(Name "not_defined")]})
    ]
  ===== ./statements/locals.lua =====
  [LocalBinding {names = ["x"]; exprs = [(String "hi LLL - 1 is very based")]};
    (CallStatement Call {prefix = (Name "print"); args = [(Name "x")]});
    LocalBinding {names = ["y"]; exprs = [10]};
    LocalBinding {names = ["z"]; exprs = [15]};
    (CallStatement
       Call {prefix = (Name "print");
         args =
         [(String "first"); (Name "y"); (String "+"); (Name "z"); (String "=");
           (Add ((Name "y"), (Name "z")))]});
    Do {
      do_block =
      { statements =
        [LocalBinding {names = ["abc"]; exprs = [10]};
          (CallStatement
             Call {prefix = (Name "print");
               args =
               [(String "in a do block"); (Name "abc"); (String "(");
                 (Name "y"); (Name "z"); (String ")")]})
          ];
        last_statement = None }}
    ]

  $ lua_eval --program --directory ./statements/
  ===== ./statements/conditionals.lua =====
  truthful 
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
