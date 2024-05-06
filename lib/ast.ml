open Core

module Name : sig
  type t [@@deriving show]

  val of_string : string -> t
  val of_string_list : string list -> t list
end = struct
  type t = string [@@deriving show]

  let of_string t = t
  let of_string_list = List.map ~f:of_string
end

type parlist =
  { args : Name.t list
  ; varargs : bool
  }

and block = statement list

and lua_function =
  { parameters : parlist
  ; block : block
  }

and name = string

and field =
  { key : expr option
  ; value : expr
  }

and expr =
  | Nil
  | True
  | False
  | Number of float
  | String of string
  | VarArgs
  | Function of lua_function
  (* Binary Operations *)
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Exponent of expr * expr
  | Mod of expr * expr
  | LT of expr * expr
  | LTE of expr * expr
  | GT of expr * expr
  | GTE of expr * expr
  | EQ of expr * expr
  | NEQ of expr * expr
  | And of expr * expr
  | Or of expr * expr
  (* Unary Operationrs *)
  | Neg of expr
  | Not of expr
  | Len of expr
  (* *)
  | Table of field list
  (* prefixexp *)
  | Name of Name.t
  | Index of expr * expr
  | Dot of expr * Name.t
  (* function call *)
  | FunctionCall of
      { prefix : expr
      ; args : expr list
      }
  | TableCall of
      { prefix : expr
      ; name : Name.t
      ; args : expr list
      }

and varlist = expr list
and exprlist = expr list

and statement =
  | Binding of varlist * exprlist
  | Call of expr
  | Return of expr
  | Do of { do_block : block }
  | While of
      { while_condition : expr
      ; while_block : block
      }
  | Repeat of
      { repeat_condition : expr
      ; repeat_block : block
      }
[@@deriving show { with_path = false }]
