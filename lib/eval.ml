open Core
open Values

let eval_string str =
  let _statements = Parse.parse str in
  assert false
;;

let rec eval_expr env expr =
  let open Value in
  let math_binop op left right =
    let left = eval_expr env left in
    let right = eval_expr env right in
    match left, right with
    | Number left, Number right -> Number (op left right)
    | _ -> Fmt.failwith "oh no, bad values"
  in
  let bool_binop op left right =
    let left = eval_expr env left in
    let right = eval_expr env right in
    Boolean (op left right)
  in
  let compare_binop op left right =
    bool_binop
      (fun l r -> op (Value.compare l r) 0)
      left
      right
  in
  let open Ast in
  match expr with
  | Nil _ -> Nil
  | True -> Boolean true
  | False -> Boolean false
  | Number float -> Number float
  | String str -> String str
  | Table fields ->
    let fields =
      List.map fields ~f:(fun { key; value } ->
        ( Option.map ~f:(eval_expr env) key
        , eval_expr env value ))
    in
    Table
      { identifier = Identifier.get_id ()
      ; numbers = NumberValues.of_fields fields
      }
  | Add (left, right) -> math_binop ( +. ) left right
  | Sub (left, right) -> math_binop ( -. ) left right
  | Mul (left, right) -> math_binop ( *. ) left right
  | Div (left, right) -> math_binop ( /. ) left right
  | EQ (left, right) -> bool_binop Value.equal left right
  | GT (left, right) -> compare_binop ( > ) left right
  | GTE (left, right) -> compare_binop ( >= ) left right
  | LT (left, right) -> compare_binop ( < ) left right
  | LTE (left, right) -> compare_binop ( <= ) left right
  | Name name -> Environment.find env name
  | CallExpr (Call { prefix; args }) ->
    let prefix = eval_expr env prefix in
    let args = List.map args ~f:(eval_expr env) in
    eval_function_call env prefix args
  | _ ->
    Fmt.failwith "unhandled expression: %a" Ast.pp_expr expr

and eval_function_call _env prefix args =
  match prefix with
  | Function func -> func.impl args
  | _ -> Fmt.failwith "prefix was not a function"
;;

let rec eval_statement env (statement : Ast.statement) =
  let open Ast in
  match statement with
  | CallStatement (Call { prefix; args }) ->
    let prefix = eval_expr env prefix in
    let args = List.map args ~f:(eval_expr env) in
    begin
      match prefix with
      | Function func -> func.impl args |> ignore
      | _ -> Fmt.failwith "prefix was not a function"
    end;
    env
  | Binding (names, exprs) ->
    let names_exprs = List.zip_exn names exprs in
    List.fold
      names_exprs
      ~init:env
      ~f:(fun env (name, value) ->
        let value = eval_expr env value in
        match name with
        | Name name -> Environment.bind env ~name ~value
        | _ -> Fmt.failwith "have to set table values here")
  | LocalBinding { names; exprs } ->
    let names_exprs = List.zip_exn names exprs in
    List.iter names_exprs ~f:(fun (name, value) ->
      let value = eval_expr env value in
      Environment.add env ~name ~value |> ignore);
    env
  | Do do_block ->
    List.fold
      do_block.statements
      ~init:(Environment.create ~parent:env ())
      ~f:eval_statement
    |> ignore;
    env
  | If { conditions = branches } ->
    List.fold_until
      branches
      ~init:(Environment.create ~parent:env ())
      ~finish:(fun env -> env)
      ~f:(fun env (condition, block) ->
        let condition = eval_expr env condition in
        match is_truthy condition with
        | true -> Stop (eval_block env block)
        | false -> Continue env)
  | statement ->
    Fmt.failwith
      "Unhandled statement: %a@."
      Ast.pp_statement
      statement

and eval_block env (block : Ast.block) =
  List.fold
    block.statements
    ~init:(Environment.create ~parent:env ())
    ~f:eval_statement
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
      eval_statement env statement)
    program
;;

let print_expr str =
  let env = Globals.All.globals in
  Fmt.pr "%a@." Value.pp (eval_string_expr env str)
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
       { identifier = 3;
         numbers =
         {1 = (Number 1.), 2 = (Number 2.), 3 = (Number 4.), 4 = (Number 8.), } }) |}];
  print_expr {| { "wow", [3] = "hello" } |};
  [%expect
    {|
    (Table
       { identifier = 4; numbers = {1 = (String "wow"), 3 = (String "hello"), } }) |}];
  ()
;;
