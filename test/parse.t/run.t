Simple Test:
  $ echo "HI"
  HI
  $ lua_ast ./test.lua
  filename: ./test.lua
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
         args =
         [(Add ((Mul ((Number 5.), (Number 5.01))),
             (Mul ((Number 37.), (Number 2.)))))
           ]});
    (CallStatement
       Call {prefix = (Name "print");
         args = [(Table [{ key = None; value = (Number 1.) }])]});
    (CallStatement
       Call {prefix = (Name "print");
         args =
         [(Table
             [{ key = None; value = (Number 1.) };
               { key = None; value = (Number 2.) };
               { key = None; value = (Number 3.) }])
           ]});
    (CallStatement
       Call {prefix = (Name "print");
         args =
         [(CallExpr Call {prefix = (Name "tostring"); args = [(Number 1.)]})]});
    (CallStatement
       Call {prefix = (Name "print");
         args =
         [(CallExpr Call {prefix = (Name "tostring"); args = [(Table [])]})]});
    LocalBinding {names = ["x"]; exprs = [(String "hi LLL - 1 is very based")]};
    (CallStatement Call {prefix = (Name "print"); args = [(Name "x")]});
    LocalBinding {names = ["y"]; exprs = [(Number 10.)]};
    LocalBinding {names = ["z"]; exprs = [(Number 15.)]};
    (CallStatement
       Call {prefix = (Name "print");
         args =
         [(String "first"); (Name "y"); (String "+"); (Name "z"); (String "=");
           (Add ((Name "y"), (Name "z")))]});
    Do {
      do_block =
      { statements =
        [LocalBinding {names = ["abc"]; exprs = [(Number 10.)]};
          (CallStatement
             Call {prefix = (Name "print");
               args =
               [(String "in a do block"); (Name "abc"); (String "(");
                 (Name "y"); (Name "z"); (String ")")]})
          ];
        last_statement = None }};
    If {
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
         args = [(String "inside:"); (Name "inside")]});
    (CallStatement
       Call {prefix = (Name "print");
         args =
         [(String "again"); (Name "y"); (String "+"); (Name "z"); (String "=");
           (Add ((Name "y"), (Name "z")))]});
    (CallStatement Call {prefix = (Name "print"); args = [(Name "abc")]})]

  $ lua_eval ./test.lua
  filename: ./test.lua
  h hello world 
  line 2 
  escaped \"  
  escaped \'  
  99.05 
  <table: 3> 
  <table: 4> 
  1 
  <table: 5> 
  hi LLL - 1 is very based 
  first 10 + 15 = 25 
  in a do block 10 ( 10 15 ) 
  truthful 
  in else block 
  inside: true 
  again 10 + 15 = 25 
  nil 
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
