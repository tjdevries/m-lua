open Core
open Bos.OS
open Mlua

let eval_file _ file =
  Fmt.pr "@.===== %s =====@." (Fpath.to_string file);
  let file =
    match File.read file with
    | Ok str -> str
    | _ -> Fmt.failwith "cannot read file %a" Fpath.pp file
  in
  let env = Eval.eval_program file in
  Fmt.pr "%a@." Environment.pp env
;;

let _ =
  let kind = Stdlib.Sys.argv.(1) in
  let mode = Stdlib.Sys.argv.(2) in
  match mode with
  | "--file" ->
    let file = Stdlib.Sys.argv.(3) |> Fpath.v in
    eval_file kind file
  | "--directory" ->
    let files =
      Stdlib.Sys.argv.(3) |> Fpath.v |> Dir.contents |> Rresult.R.get_ok
    in
    List.iter ~f:(eval_file kind) files
  | _ -> Fmt.failwith "unknown mode %s" mode
;;
