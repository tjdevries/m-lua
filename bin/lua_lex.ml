open Core
open Bos.OS
open Mlua

let print_lexed_file fpath =
  Fmt.pr "@.===== %s =====@." (Fpath.to_string fpath);
  let contents =
    match File.read fpath with
    | Ok str -> str
    | _ -> Fmt.failwith "cannot read file %a" Fpath.pp fpath
  in
  let parsed = Parse.lex contents in
  List.iter parsed ~f:(fun tok -> Fmt.pr "%s@." (Lexer.token_to_string tok))
;;

let _ =
  let mode = Stdlib.Sys.argv.(1) in
  match mode with
  | "--file" ->
    let filename = Stdlib.Sys.argv.(2) in
    let file = Fpath.v filename in
    print_lexed_file file
  | "--directory" ->
    let files =
      Stdlib.Sys.argv.(2) |> Fpath.v |> Dir.contents |> Rresult.R.get_ok
    in
    List.iter ~f:print_lexed_file files
  | _ -> Fmt.failwith "invalid mode: %s" mode
;;
