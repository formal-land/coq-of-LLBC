open Charon.LlbcAst
open Charon.Meta
open Charon.Types

open LLBC
open Util

(* coq tokens *)
type token =
  | Newline
  | Tab
  | Space
  | Def
  | DefEq
  | FullStop
  | Let
  | In
  | Id of string

let print_token = function
  | Newline -> "\n"
  | Space -> " "
  | Tab -> "  "
  | Def -> "Definition"
  | DefEq -> ":="
  | FullStop -> "."
  | Let -> "let"
  | In -> "in"
  | Id str -> str

let print_tokens toks =
  String.concat "" (List.map print_token toks)

let toks_of_type_decl (_type_dec : type_decl) : token list =
  [Def; Newline; Newline] (*TODO*)

let toks_of_fun_decl (_fun_dec : fun_decl) : token list =
  [Def; Newline; Newline] (* TODO *)

let toks_of_definition = function
  | Type_def t -> toks_of_type_decl t
  | Fun_def f -> toks_of_fun_decl f

let toks_of_compact_assoc (assoc : (file_name, definition) compact_assoc_list) : (file_name, token list) assoc_list =
  concat_compact_assoc_list
  (compact_assoc_list_map toks_of_definition assoc)

let coqify_path (file_path : string) : (string list) * string =
  let path = String.split_on_char '/' file_path in
  let rev_path = List.rev path in
  match rev_path with
  | [] -> failwith "empty path encountered."
  | name_with_rs :: rev_path_tail -> (* expecting that name_with_rs is filename with .rs extension *)
    let name = List.hd (String.split_on_char '.' name_with_rs) in
    let name_with_v = name ^ ".v" in
    let reconstructed_path = List.rev rev_path_tail in
    (reconstructed_path, name_with_v)

let coq_filename_of_file_name (file : file_name) : (string list) * string =
  match file with
  | Virtual file_path ->
      let (path, name) = coqify_path file_path in
      ("coq-files" :: "virtual" :: path, name)
  | Local file_path ->
      let (path, name) = coqify_path file_path in
      ("coq-files" :: "local" :: path, name)

let try_mkdir path =
  if not (Sys.file_exists path) then Sys.mkdir path 0o755 else ()
  (* TODO: make sure this permission makes sense *)

let get_subdirs (path : string list) : string list =
  let paths = ne_prefixes path in
  List.map (String.concat "/") paths

let try_mkdirs (path : string list) : unit =
  let subdirs = get_subdirs path in
  List.iter try_mkdir subdirs
