open Core
open Values

module U = struct
  let f impl = Function { identifier = Identifier.get_id (); impl }
end

let print_value fmt = function
  | Nil -> Format.pp_print_string fmt "nil"
  | String str -> Format.pp_print_string fmt str
  | Number num ->
    (match FloatUtils.is_int num with
     | true, num -> Format.pp_print_int fmt num
     | false, _ -> Format.pp_print_float fmt num)
  | Table _ ->
    (* TODO: print table with a stable identifier?
       or just do that for test mode... pretty annoying
       to have snap shot tests rely on the order of testing
       and the order of loading other tables before *)
    Format.fprintf fmt "<table>"
  | Boolean bool -> Format.pp_print_bool fmt bool
  | _ -> assert false
;;

let print =
  U.f (fun args ->
    Fmt.(pr "%a@." (list ~sep:(const string " ") print_value) args);
    [ Value.Nil ])
;;

let tostring =
  U.f (function
    | [ v ] -> [ String (Fmt.str "%a" print_value v) ]
    | _ -> assert false)
;;

let ipairs =
  let ipairs_iter =
    U.f (function
      | [ Table tbl; Number i ] ->
        let number = i +. 1.0 in
        (match LuaTable.findi tbl number with
         | Nil -> [ Value.Nil ]
         | value -> [ Number number; value ])
      | _ -> [ Value.Nil ])
  in
  U.f (function
    | Table tbl :: _ -> [ ipairs_iter; Table tbl; Number 0.0 ]
    | _ -> assert false)
;;

let pairs =
  U.f (function
    | Table tbl :: _ ->
      let seq = ref (LuaTable.to_seq tbl) in
      let pairs_iter =
        U.f (fun _ ->
          match Seq.uncons !seq with
          | Some ((key, value), new_seq) ->
            seq := new_seq;
            [ key; value ]
          | None -> [ Nil ])
      in
      [ pairs_iter; Table tbl; Nil ]
    | _ -> assert false)
;;

let add_function str func env =
  Environment.add env ~name:(Name.of_string str) ~value:func
;;

let globals =
  Environment.create ()
  |> add_function "print" print
  |> add_function "tostring" tostring
  |> add_function "ipairs" ipairs
  |> add_function "pairs" pairs
;;
