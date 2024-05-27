open Core
open Bos.OS
open Mlua

let print_parsed_file mode fpath =
  Fmt.pr "@.===== %s =====@." (Fpath.to_string fpath);
  let contents =
    match File.read fpath with
    | Ok str -> str
    | _ -> Fmt.failwith "cannot read file %a" Fpath.pp fpath
  in
  match mode with
  | "--expression" ->
    String.split_lines contents
    |> List.iter ~f:(fun line ->
      match line with
      | "" -> Fmt.pr "@."
      | line when String.is_prefix line ~prefix:"--" -> Fmt.pr "%s@." line
      | _ ->
        let expr = Parse.parse_expr line in
        Fmt.pr "%s@.%a@.@." line Ast.pp_expr expr)
  | "--program" ->
    let parsed = Parse.parse contents in
    Fmt.pr "%a@." Ast.pp_program parsed
  | _ -> Fmt.failwith "invalid kind: %s" mode
;;

let _ =
  let kind = Stdlib.Sys.argv.(1) in
  let mode = Stdlib.Sys.argv.(2) in
  match mode with
  | "--file" ->
    let filename = Stdlib.Sys.argv.(3) in
    let file = Fpath.v filename in
    print_parsed_file kind file
  | "--directory" ->
    let files =
      Stdlib.Sys.argv.(3) |> Fpath.v |> Dir.contents |> Rresult.R.get_ok
    in
    List.iter ~f:(print_parsed_file kind) files
  | _ -> Fmt.failwith "invalid mode: %s" mode
;;
