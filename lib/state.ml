type table = string
type t = { globals : (string * table) list }

let sandboxed () = { globals = [ "math", "<math functions here>" ] }
(* if you try to do `os.execute()`... it doesn't exist *)
