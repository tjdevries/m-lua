open Core

(* type position = Lexing.position = *)
(*   { pos_fname : string *)
(*   ; pos_lnum : int *)
(*   ; pos_bol : int *)
(*   ; pos_cnum : int *)
(*   } *)
(* [@@deriving show] *)

module FuncName = struct
  type t =
    { base : Name.t
    ; keys : Name.t list
    ; table_method : Name.t option
    }
  [@@deriving show]
end

type program = statement list

and parlist =
  { args : Name.t list
  ; varargs : bool
  }

and last_statement =
  | Return of expr list
  | Break

and block =
  { statements : statement list
  ; last_statement : last_statement option
  }

and lua_function =
  { parameters : parlist
  ; block : block
  }

and name = string
and field = expr option * expr
and exprlist = expr list

and function_call =
  | Call of prefix_expr * expr list
  | Self of prefix_expr * Name.t * expr list

and for_range =
  { name : Name.t
  ; start : expr
  ; finish : expr
  ; step : expr option
  ; for_block : block
  }

and local_function =
  { local_name : Name.t
  ; function_parameters : parlist
  ; function_block : block
  }

and var =
  | Name of Name.t
  | Index of prefix_expr * expr
  | Dot of prefix_expr * Name.t

and varlist = var list

and prefix_expr =
  | PrefixVar of var
  | PrefixCall of function_call
  | PrefixParens of expr

and expr =
  | Nil
  | True
  | False
  | Number of float [@printer FloatUtils.lua_print]
  | String of string
  | VarArgs
  | Function of lua_function
  (* Binary Operations *)
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Concat of expr * expr
  | Exponent of expr * expr
  | Mod of (expr * expr)
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
  | Var of var
  | PrefixExpr of prefix_expr
  (* function call *)
  | CallExpr of function_call

and statement =
  | Binding of varlist * exprlist
  | LocalBinding of Name.t list * expr list
  | CallStatement of function_call
  | Do of block
  | While of expr * block
  | Repeat of block * expr
  | If of (expr * block) list
  | ForNames of Name.t list * expr list * block
  | ForRange of for_range
  | FunctionStatement of
      { function_name : FuncName.t
      ; function_parameters : parlist
      ; function_block : block
      }
  | LocalFunction of local_function
[@@deriving show { with_path = false }]

(* let pp_expr fmt = function *)
(*   | Number num -> Format.pp_print_float fmt num *)
(*   | expr -> pp_expr fmt expr *)
(* ;; *)

let parlist_opt p =
  match p with
  | Some p -> p
  | None -> { args = []; varargs = false }
;;

let exprlist_opt = Option.value ~default:[]
