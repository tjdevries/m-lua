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

let print =
  Value.Function
    { identifier = Identifier.get_id ()
    ; impl =
        (fun args ->
          (* Fmt.list *)
          List.iter args ~f:(Fmt.pr "%a " print_value);
          Fmt.pr "@.";
          [ Value.Nil ])
    }
;;

let tostring =
  Function
    { identifier = Identifier.get_id ()
    ; impl =
        (function
          | [ v ] -> [ String (Fmt.str "%a" print_value v) ]
          | _ -> assert false)
    }
;;

let ipairs =
  let ipairs_iter =
    Function
      { identifier = Identifier.get_id ()
      ; impl =
          (function
            | [ Table tbl; Number i ] ->
              let number = i +. 1.0 in
              (match LuaTable.findi tbl number with
               | Nil -> [ Value.Nil ]
               | value -> [ Number number; value ])
            | _ -> [ Value.Nil ])
      }
  in
  Function
    { identifier = Identifier.get_id ()
    ; impl =
        (function
          | Table tbl :: _ ->
            [ ipairs_iter; Table tbl; Number 0.0 ]
          | _ -> assert false)
    }
;;

let pairs =
  Function
    { identifier = Identifier.get_id ()
    ; impl =
        (function
          | Table tbl :: _ -> assert false
          | _ -> assert false)
    }
;;

let add_function str func env =
  Environment.add env ~name:(Name.of_string str) ~value:func
;;

let globals =
  Environment.create ()
  |> add_function "print" print
  |> add_function "tostring" tostring
  |> add_function "ipairs" ipairs
;;
