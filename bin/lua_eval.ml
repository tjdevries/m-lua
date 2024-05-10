open Core
open Bos.OS
open Mlua

let print_parsed_file _ file =
  Fmt.pr "===== %s =====@." (Fpath.to_string file);
  let file =
    match File.read file with
    | Ok str -> str
    | _ -> assert false
  in
  let env = Eval.eval_program file in
  Fmt.pr "%a@." Environment.pp env
;;

let _ =
  let kind = Stdlib.Sys.argv.(1) in
  let mode = Stdlib.Sys.argv.(2) in
  match mode with
  | "--file" -> assert false
  | "--directory" ->
    let files =
      Stdlib.Sys.argv.(3)
      |> Fpath.v
      |> Dir.contents
      |> Rresult.R.get_ok
    in
    List.iter ~f:(print_parsed_file kind) files
  | _ -> assert false
;;
