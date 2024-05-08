open Sexplib.Std

type t = int [@@deriving show, eq, ord, sexp]

let hash x = x
let current_id = ref 0

let get_id () =
  incr current_id;
  !current_id
;;
