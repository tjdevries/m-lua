open Core
module Value = Values.Value

(* print -> Identifier.t -> Value.t *)
(* print -> Value.t *)

module NameLookup = struct
  type t = (Name.t, Value.t) Hashtbl.t

  let pp fmt _ = Format.pp_print_string fmt "<lookup tbl>"
end

type t =
  { locals : NameLookup.t
  ; parent : t option
  }
[@@deriving show]

let create ?parent () = { locals = Hashtbl.create (module Name); parent }

(* Should add error if it's already set? Maybe we should
   have `set` instead *)
let add t ~name ~value =
  Hashtbl.set t.locals ~key:name ~data:value;
  t
;;

let rec find t name =
  match Hashtbl.find t.locals name, t.parent with
  | Some name, _ -> name
  | None, Some parent -> find parent name
  | None, None -> Value.Nil
;;

let rec bind t ~name ~value =
  match Hashtbl.find t.locals name, t.parent with
  | Some _, _ -> add t ~name ~value
  | None, Some parent -> bind parent ~name ~value
  | None, None -> bind t ~name ~value
;;
