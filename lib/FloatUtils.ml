let is_int f =
  let floored = Stdlib.floor f in
  Float.equal f floored, Float.to_int floored
;;

let lua_print fmt f =
  match is_int f with
  | true, num -> Format.pp_print_int fmt num
  | false, _ -> Format.pp_print_float fmt f
;;
