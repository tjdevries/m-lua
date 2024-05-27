Execute Test Suite:
  $ lua_eval --program --directory ./suite/
  
  ===== ./suite/alias_alloc.lua =====
  { Environment.locals = <lookup tbl>; parent = None }
  
  ===== ./suite/ack_notail.lua =====
  { Environment.locals = <lookup tbl>; parent = None }
  
  ===== ./suite/fac.lua =====
  3628800
  { Environment.locals = <lookup tbl>; parent = None }
  
  ===== ./suite/ack.lua =====
  { Environment.locals = <lookup tbl>; parent = None }
  
  ===== ./suite/api_call.lua =====
  { Environment.locals = <lookup tbl>; parent = None }
  
  ===== ./suite/argcheck.lua =====
  Uncaught exception:
    
    (Failure "oh no, bad values")
  
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Fmt.failwith in file "src/fmt.ml" (inlined), line 25, characters 19-36
  Called from Mlua__Eval.eval_index in file "lib/eval.ml", line 119, characters 11-43
  Called from Mlua__Eval.eval_prefix in file "lib/eval.ml", line 85, characters 27-43
  Called from Mlua__Eval.first_prefix in file "lib/eval.ml", line 93, characters 2-24
  Called from Mlua__Eval.eval_prefix in file "lib/eval.ml", line 87, characters 17-40
  Called from Mlua__Eval.first_expr in file "lib/eval.ml", line 81, characters 2-20
  Called from Mlua__Eval.eval_expr.bool_binop in file "lib/eval.ml", line 24, characters 15-34
  Called from Mlua__Eval.first_expr in file "lib/eval.ml", line 81, characters 2-20
  Called from Base__List.count_map in file "src/list.ml", line 479, characters 13-17
  Called from Base__List.map in file "src/list.ml" (inlined), line 510, characters 15-31
  Called from Mlua__Eval.eval_statement in file "lib/eval.ml", line 154, characters 15-48
  Called from Mlua__Eval.eval_program.(fun) in file "lib/eval.ml", line 304, characters 6-34
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Dune__exe__Lua_eval.eval_file in file "bin/lua_eval.ml", line 12, characters 12-34
  Called from Base__List0.iter in file "src/list0.ml", line 60, characters 4-7
  Called from Dune__exe__Lua_eval in file "bin/lua_eval.ml", line 27, characters 4-39
  [2]
