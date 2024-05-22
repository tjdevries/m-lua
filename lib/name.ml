open Core

type t = string [@@deriving show, eq, ord, sexp]

let hash = hash_string
let of_string t = t
let of_string_list = List.map ~f:of_string
let to_string t = t
