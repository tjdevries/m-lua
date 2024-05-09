open Core
open Bos.OS
open Mlua

let _ =
  let filename = Stdlib.Sys.argv.(1) in
  let _ = Fmt.pr "filename: %s@." filename in
  let file = Fpath.v filename in
  let file =
    match File.read file with
    | Ok str -> str
    | _ -> assert false
  in
  let parsed = Parse.parse file in
  Fmt.pr "%a@." Ast.pp_program parsed;
  ()
;;
