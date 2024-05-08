open Core
module Value = Values.Value

type t =
  { identifiers : (Identifier.t, Value.t) Hashtbl.t
  ; parent : t option
  }

let create () =
  { identifiers = Hashtbl.create (module Identifier); parent = None }
;;
