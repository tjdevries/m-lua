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
    | Number left, Number right ->
      [ Number (op left right) ]
    | _ -> Fmt.failwith "oh no, bad values"
  in
  let bool_binop op left right =
    let left = first_expr env left in
    let right = first_expr env right in
    [ Boolean (op left right) ]
  in
  let compare_binop op left right =
    bool_binop
      (fun l r -> op (Value.compare l r) 0)
      left
      right
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
        ( Option.map ~f:(first_expr env) key
        , first_expr env value ))
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
  | EQ (left, right) -> bool_binop Value.equal left right
  | GT (left, right) -> compare_binop ( > ) left right
  | GTE (left, right) -> compare_binop ( >= ) left right
  | LT (left, right) -> compare_binop ( < ) left right
  | LTE (left, right) -> compare_binop ( <= ) left right
  | Name name -> [ Environment.find env name ]
  | CallExpr (Call (prefix, args)) ->
    let prefix = first_expr env prefix in
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
  | Index (prefix, index) ->
    eval_index_expr env prefix index
  | Dot (prefix, name) -> eval_dot_expr env prefix name
  | _ ->
    Fmt.failwith "unhandled expression: %a" Ast.pp_expr expr

and first_expr env expr =
  eval_expr env expr |> List.hd |> Option.value ~default:Nil

and eval_index env table index =
  let tbl = first_expr env table in
  let tbl =
    match tbl with
    | Table tbl -> tbl
    | _ -> Fmt.failwith "oh no, bad values"
  in
  match index with
  | Number number -> [ LuaTable.findi tbl number ]
  | index -> [ LuaTable.find tbl index ]

and eval_index_expr env prefix index =
  let index = first_expr env index in
  eval_index env prefix index

and eval_dot_expr env prefix name =
  let name = Name.to_string name in
  let value = Value.String name in
  eval_index env prefix value

and set_names env names values =
  let zipped, remainder =
    List.zip_with_remainder names values
  in
  List.iter zipped ~f:(fun (name, value) ->
    Environment.add env ~name ~value |> ignore);
  match remainder with
  | Some (First names) ->
    List.iter names ~f:(fun name ->
      Environment.add env ~name ~value:Values.Nil |> ignore)
  | _ -> ()

and eval_function_call _env prefix args =
  match prefix with
  | Function func -> func.impl args
  | _ -> Fmt.failwith "prefix was not a function"

and eval_statement env statement : control_flow =
  let open Ast in
  match statement with
  | CallStatement (Call (prefix, args)) ->
    let prefix = first_expr env prefix in
    let args = List.map args ~f:(first_expr env) in
    begin
      match prefix with
      | Function func ->
        func.impl args |> ignore;
        NoReturn
      | _ -> Fmt.failwith "prefix was not a function"
    end
  | Binding (names, exprs) ->
    let names_exprs = List.zip_exn names exprs in
    List.iter names_exprs ~f:(fun (name, value) ->
      let value = first_expr env value in
      match name with
      | Name name ->
        Environment.bind env ~name ~value |> ignore
      | _ -> Fmt.failwith "have to set table values here");
    NoReturn
  | LocalBinding (names, exprs) ->
    let names_exprs = List.zip_exn names exprs in
    List.iter names_exprs ~f:(fun (name, value) ->
      let value = first_expr env value in
      Environment.add env ~name ~value |> ignore);
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
  | ForNames (names, exprs, block) ->
    eval_for_names env names exprs block
  | statement ->
    Fmt.failwith
      "Unhandled statement: %a@."
      Ast.pp_statement
      statement

and eval_for_names env names exprs block =
  (* TODO: Handle not just having one expression *)
  let env = Environment.create ~parent:env () in
  let exprs = eval_expr env (List.hd_exn exprs) in
  let iter, state, value =
    match exprs with
    | iter :: state :: value :: _ -> iter, state, value
    | _ -> assert false
  in
  let rec loop value =
    let variables =
      eval_function_call env iter [ state; value ]
    in
    match variables with
    | Nil :: _ -> NoReturn
    | value :: rest as variables ->
      set_names env names variables;
      let _ = eval_block env block in
      loop value
    | _ -> assert false
  in
  loop value

and eval_for_range
  env
  { name; start; finish; step; for_block }
  =
  let open Float in
  let rec loop start stop step f =
    if (step > 0.0 && start <= stop)
       || (step < 0.0 && start >= stop)
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
    Option.value_map
      step
      ~default:(Values.Number 1.0)
      ~f:(first_expr env)
  in
  begin
    match start, finish, step with
    | Number start, Number finish, Number step ->
      loop start finish step (fun index ->
        let _env =
          Environment.add
            env
            ~name
            ~value:(Values.Number index)
        in
        eval_block env for_block)
    | _ -> assert false
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
           let values =
             List.map exprs ~f:(first_expr env)
           in
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
  Fmt.pr
    "%a@."
    Value.pp
    (eval_string_expr env str |> List.hd_exn)
;;

let%expect_test "expr:addition" =
  print_expr "5";
  [%expect {| (Number 5.) |}];
  print_expr "5 + 10";
  [%expect {| (Number 15.) |}];
  print_expr "5 + 1 / 10 + 13 * 2";
  [%expect {| (Number 31.1) |}];
  print_expr {| "hello world" |};
  [%expect {| (String "hello world") |}];
  print_expr {| "hello" == "hello" |};
  [%expect {| (Boolean true) |}];
  print_expr {| 5.0 == 5 * 1 * 2 / 2 |};
  [%expect {| (Boolean true) |}];
  print_expr {| 5.1 == 5 * 1 * 2 / 2 |};
  [%expect {| (Boolean false) |}];
  print_expr {| "5.0" == 5 * 1 * 2 / 2 |};
  [%expect {| (Boolean false) |}];
  print_expr {| 5.1 > 5 * 1 * 2 / 2 |};
  [%expect {| (Boolean true) |}];
  print_expr {| nil |};
  [%expect {| Nil |}];
  print_expr {| { 1, 2, 4, 8 } |};
  [%expect
    {|
    (Table
       { identifier = 6;
         numbers =
         {1 = (Number 1.), 2 = (Number 2.), 3 = (Number 4.), 4 = (Number 8.), };
         values = <value tbl> }) |}];
  print_expr {| { "wow", [3] = "hello" } |};
  [%expect
    {|
    (Table
       { identifier = 7; numbers = {1 = (String "wow"), 3 = (String "hello"), };
         values = <value tbl> }) |}];
  ()
;;
