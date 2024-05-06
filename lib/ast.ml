type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Exp
  | Mod
  | Concat
  | LT
  | LTE
  | GT
  | GTE
  | EQ
  | NEQ
  | And
  | Or

and unop =
  | Neg
  | Not
  | Len

and expr =
  | Integer of int
  | Binop of expr * binop * expr
  | Name of string

and varlist = string list
and exprlist = expr list

and statement = Binding of varlist * exprlist
[@@deriving show { with_path = false }]
