type t = string [@@deriving show, eq, ord, sexp]

val hash : t -> int
val of_string : string -> t
val of_string_list : string list -> t list
val to_string : t -> string
