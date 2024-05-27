open Core
open Values

type control_flow =
  | Return of Value.t list
  | Break
  | NoReturn

let result_or_nil = function
  | Return vs -> vs
  | _ -> [ Nil ]
;;

let rec eval_expr env expr : Value.t list =
  let open Value in
  let math_binop op left right =
    let left = first_expr env left in
    let right = first_expr env right in
    match left, right with
    | Number left, Number right -> [ Number (op left right) ]
    | _ -> Fmt.failwith "oh no, bad values"
  in
  let bool_binop op left right =
    let left = first_expr env left in
    let right = first_expr env right in
    [ Boolean (op left right) ]
  in
  let compare_binop op left right =
    bool_binop (fun l r -> op (Value.compare l r) 0) left right
  in
  let open Ast in
  match expr with
  | Nil -> [ Nil ]
  | True -> [ Boolean true ]
  | False -> [ Boolean false ]
  | Number float -> [ Number float ]
  | String str -> [ String str ]
  | Table fields ->
    let fields =
      List.map fields ~f:(fun (key, value) ->
        Option.map ~f:(first_expr env) key, first_expr env value)
    in
    [ Table
        { identifier = Identifier.get_id ()
        ; numbers = NumberValues.of_fields fields
        ; values = ValueTbl.of_fields fields
        }
    ]
  | Add (left, right) -> math_binop ( +. ) left right
  | Sub (left, right) -> math_binop ( -. ) left right
  | Mul (left, right) -> math_binop ( *. ) left right
  | Div (left, right) -> math_binop ( /. ) left right
  | Mod (left, right) -> math_binop Float.mod_float left right
  | EQ (left, right) -> bool_binop Value.equal left right
  | And (left, right) -> eval_and env left right
  | GT (left, right) -> compare_binop ( > ) left right
  | GTE (left, right) -> compare_binop ( >= ) left right
  | LT (left, right) -> compare_binop ( < ) left right
  | LTE (left, right) -> compare_binop ( <= ) left right
  | Var var -> [ eval_var env var ]
  | PrefixExpr prefix -> eval_prefix env prefix
  | CallExpr (Call (prefix, args)) ->
    Fmt.pr "CALLING FUNCTION NOW@.";
    let prefix = first_prefix env prefix in
    (* TODO: I think this won't work with last values and weird ellipsis *)
    let args = List.map args ~f:(first_expr env) in
    eval_function_call env prefix args
  | Function { block; parameters } ->
    [ Function
        { identifier = Identifier.get_id ()
        ; impl =
            (fun arg_values ->
              let env = Environment.create ~parent:env () in
              set_names env parameters.args arg_values;
              eval_block env block |> result_or_nil)
        }
    ]
  | _ -> Fmt.failwith "unhandled expression: %a" Ast.pp_expr expr

and first_expr env expr =
  eval_expr env expr |> List.hd |> Option.value ~default:Value.Nil

and eval_prefix env prefix =
  match prefix with
  | Ast.PrefixVar var -> [ eval_var env var ]
  | Ast.PrefixCall (Call (prefix, args)) ->
    let prefix = first_prefix env prefix in
    eval_function_call env prefix (List.map args ~f:(first_expr env))
  | Ast.PrefixCall (Self _) -> Fmt.failwith "self call not implemented"
  | Ast.PrefixParens expr -> eval_expr env expr

and first_prefix env prefix : Value.t =
  eval_prefix env prefix |> List.hd |> Option.value ~default:Value.Nil

and eval_var env var =
  match var with
  | Name name -> Environment.find env name
  | Index (prefix, expr) ->
    let prefix = first_prefix env prefix in
    eval_index env prefix (first_expr env expr)
  | Dot (prefix, name) ->
    let prefix = first_prefix env prefix in
    eval_dot env prefix name

and eval_dot env prefix name = eval_index env prefix (Value.String name)

and eval_and env left right =
  let left = first_expr env left in
  match left with
  | value when is_truthy value ->
    let right = first_expr env right in
    [ Boolean (Values.value_and left right) ]
  | _ -> [ Boolean false ]

and eval_index _env table index =
  let tbl =
    match table with
    | Table tbl -> tbl
    | _ -> Fmt.failwith "oh no, bad values"
  in
  match index with
  | Value.Number number -> LuaTable.findi tbl number
  | index -> LuaTable.find tbl index

and eval_index_expr env prefix index =
  (* let index = first_expr env index in *)
  eval_index env prefix index

and eval_dot_expr env prefix name =
  let name = Name.to_string name in
  let value = Value.String name in
  eval_index env prefix value

and set_names env names values =
  let zipped, remainder = List.zip_with_remainder names values in
  List.iter zipped ~f:(fun (name, value) ->
    Environment.add env ~name ~value |> ignore);
  match remainder with
  | Some (First names) ->
    List.iter names ~f:(fun name ->
      Environment.add env ~name ~value:Value.Nil |> ignore)
  | _ -> ()

and eval_function_call _env (prefix : Value.t) args =
  match prefix with
  | Function func -> func.impl args
  | _ -> Fmt.failwith "prefix was not a function"

and eval_statement env statement : control_flow =
  let open Ast in
  match statement with
  | CallStatement (Call (prefix, args)) ->
    let prefix = first_prefix env prefix in
    let args = List.map args ~f:(first_expr env) in
    begin
      match prefix with
      | Function func ->
        func.impl args |> ignore;
        NoReturn
      | _ -> Fmt.failwith "prefix was not a function"
    end
  | Binding (names, exprs) -> eval_binding env names exprs
  | LocalBinding (names, exprs) ->
    set_names env names @@ List.map exprs ~f:(first_expr env);
    (* let names_exprs = List.zip_exn names exprs in *)
    (* List.iter names_exprs ~f:(fun (name, value) -> *)
    (*   let value = first_expr env value in *)
    (*   Environment.add env ~name ~value |> ignore); *)
    NoReturn
  | Do do_block ->
    let env = Environment.create ~parent:env () in
    eval_block env do_block
  | If branches ->
    let env = Environment.create ~parent:env () in
    List.fold_until
      branches
      ~init:()
      ~finish:(fun _ -> NoReturn)
      ~f:(fun _ (condition, block) ->
        let condition = first_expr env condition in
        match is_truthy condition with
        | true -> Stop (eval_block env block)
        | false -> Continue ())
  | ForRange for_range -> eval_for_range env for_range
  | ForNames (names, exprs, block) -> eval_for_names env names exprs block
  | LocalFunction local_function -> eval_local_function env local_function
  | statement ->
    Fmt.failwith "Unhandled statement: %a@." Ast.pp_statement statement

and eval_binding env names exprs =
  let values = List.map exprs ~f:(first_expr env) in
  let zipped, _remainder = List.zip_with_remainder names values in
  List.iter zipped ~f:(fun (lhs, value) ->
    match lhs with
    | Name name -> Environment.bind env ~name ~value |> ignore
    | Index (prefix, index) ->
      let prefix = first_prefix env prefix in
      let index = first_expr env index in
      (match prefix with
       | Table tbl -> LuaTable.set tbl ~key:index ~value
       | _ -> Fmt.failwith "NOT A TABLE, CANT SET")
    | Dot (_prefix, _name) -> Fmt.failwith "DOT");
  NoReturn

and eval_local_function env { local_name; function_parameters; function_block } =
  let func_env = Environment.create ~parent:env () in
  let value =
    Utils.f (fun args ->
      set_names func_env function_parameters.args args;
      eval_block func_env function_block |> result_or_nil)
  in
  Environment.add env ~name:local_name ~value |> ignore;
  NoReturn

and eval_for_names env names exprs block =
  (* TODO: Handle not just having one expression *)
  let env = Environment.create ~parent:env () in
  let iter, state, value =
    match exprs with
    | [ expr ] ->
      let exprs = eval_expr env expr in
      (match exprs with
       | iter :: state :: value :: _ -> iter, state, value
       | invalid ->
         Fmt.failwith
           "for_names: expected 3 values (%a)"
           (Fmt.Dump.list Value.pp)
           invalid)
    | iter :: state :: value :: _ ->
      first_expr env iter, first_expr env state, first_expr env value
    | _ -> Fmt.failwith "for_names: unknown exprs match"
  in
  let rec loop value =
    let variables = eval_function_call env iter [ state; value ] in
    match variables with
    | Nil :: _ -> NoReturn
    | value :: _ as variables ->
      set_names env names variables;
      let _ = eval_block env block in
      loop value
    | _ -> Fmt.failwith "for_names: expected variable for function call result"
  in
  loop value

and eval_for_range env { name; start; finish; step; for_block } =
  let open Float in
  let rec loop (start : float) (stop : float) (step : float) f =
    if (step > 0.0 && start <= stop) || (step < 0.0 && start >= stop)
    then begin
      match f start with
      | NoReturn -> loop (start + step) stop step f
      | value -> value
    end
    else NoReturn
  in
  let env = Environment.create ~parent:env () in
  let start = first_expr env start in
  let finish = first_expr env finish in
  let step =
    Option.value_map step ~default:(Value.Number 1.0) ~f:(first_expr env)
  in
  begin
    match start, finish, step with
    | Number start, Number finish, Number step ->
      loop start finish step (fun index ->
        let _env = Environment.add env ~name ~value:(Value.Number index) in
        eval_block env for_block)
    | _ -> Fmt.failwith "for_range: expected numbers"
  end

and eval_block parent (block : Ast.block) : control_flow =
  let env = Environment.create ~parent () in
  List.fold_until
    block.statements
    ~init:NoReturn
    ~finish:(function
      | Return values -> Return values
      | Break -> Break
      | NoReturn ->
        (match block.last_statement with
         | Some (Return exprs) ->
           let values = List.map exprs ~f:(first_expr env) in
           Return values
         | Some Break -> Break
         | None -> NoReturn))
    ~f:(fun _ stmt ->
      match eval_statement env stmt with
      | Return values -> Stop (Return values)
      | Break -> Stop Break
      | NoReturn -> Continue NoReturn)
;;

let eval_string_expr env str =
  let expr = Parse.parse_expr str in
  eval_expr env expr
;;

let eval_program str =
  let program = Parse.parse str in
  List.fold
    ~init:Globals.All.globals
    ~f:(fun env statement ->
      (* Fmt.pr "statement: %a@." Ast.pp_statement statement; *)
      eval_statement env statement |> ignore;
      env)
    program
;;

let print_expr str =
  let env = Globals.All.globals in
  Fmt.pr "%a@." Value.pp (eval_string_expr env str |> List.hd_exn)
;;
