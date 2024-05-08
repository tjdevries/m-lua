open Core
module Value = Values.Value
module NumberValues = Values.NumberValues

let eval_string str =
  let _statements = Parse.parse str in
  assert false
;;

let rec eval_expr expr =
  let open Value in
  let math_binop op left right =
    let left = eval_expr left in
    let right = eval_expr right in
    match left, right with
    | Number left, Number right -> Number (op left right)
    | _ -> Fmt.failwith "oh no, bad values"
  in
  let bool_binop op left right =
    let left = eval_expr left in
    let right = eval_expr right in
    Boolean (op left right)
  in
  let compare_binop op left right =
    bool_binop (fun l r -> op (Value.compare l r) 0) left right
  in
  let open Ast in
  match expr with
  | Nil -> Nil
  | True -> Boolean true
  | False -> Boolean false
  | Number float -> Number float
  | String str -> String str
  | Table fields ->
    let fields =
      List.map fields ~f:(fun { key; value } ->
        Option.map ~f:eval_expr key, eval_expr value)
    in
    Table { numbers = NumberValues.of_fields fields }
  | Add (left, right) -> math_binop ( +. ) left right
  | Sub (left, right) -> math_binop ( -. ) left right
  | Mul (left, right) -> math_binop ( *. ) left right
  | Div (left, right) -> math_binop ( /. ) left right
  | EQ (left, right) -> bool_binop Value.equal left right
  | GT (left, right) -> compare_binop ( > ) left right
  | GTE (left, right) -> compare_binop ( >= ) left right
  | LT (left, right) -> compare_binop ( < ) left right
  | LTE (left, right) -> compare_binop ( <= ) left right
  | _ -> Fmt.failwith "unhandled expression"
;;

let eval_statement env statement =
  let open Ast in
  match statement with
  | Call _ -> env
  | _ -> env
;;

let eval_string_expr str =
  let expr = Parse.parse_expr str in
  eval_expr expr
;;

let eval_program str =
  let program = Parse.parse str in
  let env = Environment.create () in
  List.fold ~init:env ~f:(fun acc _statement -> acc) program
;;

let print_expr str = Fmt.pr "%a@." Value.pp (eval_string_expr str)

let%expect_test "expr:addition" =
  print_expr "5";
  [%expect {| (Number 5.) |}];
  print_expr "5 + 10";
  [%expect {| (Number 15.) |}];
  print_expr "5 + 1 / 10 + 13 * 2";
  [%expect {| (Number 31.1) |}];
  print_expr {| "hello world" |};
  [%expect {| (String "\"hello world\"") |}];
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
    Table {
      numbers =
      {1 = (Number 1.), 2 = (Number 2.), 3 = (Number 4.), 4 = (Number 8.), }} |}];
  print_expr {| { "wow", [3] = "hello" } |};
  [%expect
    {| Table {numbers = {1 = (String "\"wow\""), 3 = (String "\"hello\""), }} |}];
  ()
;;
