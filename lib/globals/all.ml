open Core
open Values

let print_value fmt = function
  | Nil -> Format.pp_print_string fmt "nil"
  | String str -> Format.pp_print_string fmt str
  | Number num ->
    (match FloatUtils.is_int num with
     | true, num -> Format.pp_print_int fmt num
     | false, _ -> Format.pp_print_float fmt num)
  | Table tbl ->
    Format.fprintf
      fmt
      "<table: %a>"
      Identifier.pp
      tbl.identifier
  | Boolean bool -> Format.pp_print_bool fmt bool
  | _ -> assert false
;;

let print_function =
  Value.Function
    { identifier = Identifier.get_id ()
    ; impl =
        (fun args ->
          (* Fmt.list *)
          List.iter args ~f:(Fmt.pr "%a " print_value);
          Fmt.pr "@.";
          Value.Nil)
    }
;;

let tostring_function =
  Function
    { identifier = Identifier.get_id ()
    ; impl =
        (function
          | [ v ] -> String (Fmt.str "%a" print_value v)
          | _ -> assert false)
    }
;;

let globals =
  Environment.create ()
  |> Environment.add
       ~name:(Name.of_string "print")
       ~value:print_function
  |> Environment.add
       ~name:(Name.of_string "tostring")
       ~value:tostring_function
;;
