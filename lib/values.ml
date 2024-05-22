open Core
open Sexplib.Std

(* this is such a bad idea i'm pretty sure LOL *)
let address (obj : 'a) =
  let id = Obj.repr obj in
  (Obj.magic id : int)
;;

module LuaFormatter = struct
  let pp_float fmt f =
    match FloatUtils.is_int f with
    | true, int -> Format.pp_print_int fmt int
    | false, _ -> Format.pp_print_float fmt f
  ;;
end

module rec NumberValues : sig
  type t [@@deriving sexp]

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Formatter.t -> t -> unit
  val of_fields : (Value.t option * Value.t) list -> t
  val find : t -> float -> Value.t
end = struct
  open Core

  type t = (float, Value.t) Hashtbl.t

  let sexp_of_t t =
    Hashtbl.to_alist t |> [%sexp_of: (float * Value.t) list]
  ;;

  let t_of_sexp sexp =
    let alist = [%of_sexp: (float * Value.t) list] sexp in
    let tbl : t = Hashtbl.create (module Float) in
    List.iter alist ~f:(fun (key, value) ->
      Hashtbl.add tbl ~key ~data:value |> ignore);
    tbl
  ;;

  let compare a b = Stdlib.compare (address a) (address b)
  let equal a b = compare a b = 0

  let pp fmt (t : t) =
    Format.pp_print_string fmt "{";
    Hashtbl.iteri t ~f:(fun ~key ~data ->
      let _, _ = key, data in
      Format.fprintf
        fmt
        "%a = %a, "
        LuaFormatter.pp_float
        key
        Value.pp
        data);
    Format.pp_print_string fmt "}";
    ()
  ;;

  let of_fields (fields : (Value.t option * Value.t) list) =
    let tbl : t = Hashtbl.create (module Float) in
    let idx = ref 0.0 in
    List.iter fields ~f:(fun (key, value) ->
      match key with
      | Some (Number key) ->
        Hashtbl.add tbl ~key ~data:value |> ignore
      | Some _ -> ()
      | None ->
        idx := !idx +. 1.0;
        Hashtbl.add tbl ~key:!idx ~data:value |> ignore);
    tbl
  ;;

  let find t key =
    Hashtbl.find t key |> Option.value ~default:Value.Nil
  ;;
end

and ValueTbl : sig
  type t [@@deriving sexp]

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Formatter.t -> t -> unit
  val of_fields : (Value.t option * Value.t) list -> t
  val find : t -> Value.t -> Value.t
end = struct
  open Core

  type t = (Value.t, Value.t) Hashtbl.t

  let sexp_of_t t =
    Hashtbl.to_alist t
    |> [%sexp_of: (Value.t * Value.t) list]
  ;;

  let t_of_sexp sexp =
    let alist = [%of_sexp: (Value.t * Value.t) list] sexp in
    let tbl : t = Hashtbl.create (module Value) in
    List.iter alist ~f:(fun (key, value) ->
      Hashtbl.add tbl ~key ~data:value |> ignore);
    tbl
  ;;

  let compare a b = Stdlib.compare (address a) (address b)
  let equal a b = compare a b = 0
  let pp fmt _ = Format.pp_print_string fmt "<value tbl>"

  let of_fields (fields : (Value.t option * Value.t) list) =
    let tbl : t = Hashtbl.create (module Value) in
    List.iter fields ~f:(fun (key, value) ->
      match key with
      | Some (Number key) -> ()
      | Some key ->
        Hashtbl.add tbl ~key ~data:value |> ignore
      | None -> ());
    tbl
  ;;

  let find t key =
    Hashtbl.find t key |> Option.value ~default:Value.Nil
  ;;
end

and LuaTable : sig
  type t =
    { identifier : Identifier.t
    ; numbers : NumberValues.t
    ; values : ValueTbl.t
    }
  [@@deriving show, ord, eq, sexp]

  val find : t -> Value.t -> Value.t
  val findi : t -> float -> Value.t
end = struct
  type t =
    { identifier : Identifier.t
    ; numbers : NumberValues.t
    ; values : ValueTbl.t
    }
  [@@deriving show { with_path = false }, ord, eq, sexp]

  let find t key = ValueTbl.find t.values key
  let findi t key = NumberValues.find t.numbers key

  (* (* let show *) *)
  (* let pp fmt t = Identifier.pp fmt t.identifier *)
  (* let show t = Identifier.show t.identifier *)
  (* let compare a b = compare (address a) (address b) *)
  (* let equal a b = compare a b = 0 *)
end

and LuaFunction : sig
  type t =
    { identifier : Identifier.t
    ; impl : Value.t list -> Value.t list
    }
  [@@deriving show, ord, eq, sexp]
end = struct
  type t =
    { identifier : Identifier.t
    ; impl : Value.t list -> Value.t list
    }
  [@@deriving sexp]

  (* let show *)
  let pp fmt t = Identifier.pp fmt t.identifier
  let show t = Identifier.show t.identifier
  let compare a b = compare (address a) (address b)
  let equal a b = compare a b = 0
end

and Value : sig
  type t =
    | Nil
    | Boolean of bool
    | Number of float
    | String of string
    | Table of LuaTable.t
    | Function of LuaFunction.t
    | Userdata
    | Thread
  [@@deriving show, ord, eq, sexp]

  val hash : t -> int
end = struct
  type t =
    | Nil
    | Boolean of bool
    | Number of float
    | String of string
    | Table of LuaTable.t
    | Function of LuaFunction.t
    | Userdata
    | Thread
  [@@deriving show { with_path = false }, ord, eq, sexp]

  let hash t = 0
end

include Value

let is_truthy = function
  | Nil | Boolean false -> false
  | _ -> true
;;
