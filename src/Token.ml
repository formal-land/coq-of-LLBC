open Charon.LlbcAst
open Charon.Meta
open Charon.Types
open Charon.Names

open LLBC
open Util

(* coq tokens and whitespace *)
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
  | Colon
  | Set
  | Record
  | Axiom
  | Arrow
  | LCurly
  | RCurly
  | Semicolon
  | LParen
  | RParen
  | Bool
  | Ascii
  | Empty_set
  | Z
  | String
  | Array
  | Inductive
  | Vert

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
  | Colon -> ":"
  | Set -> "Set"
  | Record -> "Record"
  | Axiom -> "Axiom"
  | Arrow -> "->"
  | LCurly -> "{"
  | RCurly -> "}"
  | Semicolon -> ";"
  | LParen -> "("
  | RParen -> ")"
  | Bool -> "bool"
  | Ascii -> "ascii"
  | Empty_set -> "Empty_set"
  | Z -> "Z"
  | String -> "string"
  | Array -> "array"
  | Inductive -> "Inductive"
  | Vert -> "|"

let print_tokens toks =
  String.concat "" (List.map print_token toks)

let last_str name =
  let elem = last name in
  match elem with
  | Ident str -> str
  | Disambiguator _ -> failwith "Disambiguator encountered."

(* token of name, ignoring path *)
let tok_of_name name =
  Id (last_str name)

let tok_of_ty_var (var : type_var) =
  Id var.name

let toks_of_var (v : var) =
  match v.name with
  | None -> []
  | Some name -> [Id name]

let toks_of_ty_vars (vars : type_var list) =
  intersperse Space (List.map tok_of_ty_var vars)

let toks_of_vars vs =
  intersperse Space (List.concat_map toks_of_var vs)

let rec arity_n_set_sig n =
  if n = 0 then [Set] else
  [Set; Space; Arrow; Space] @ arity_n_set_sig (n-1)

let rec toks_of_ty (ty_params : type_var list) = function
  | Adt (_id, _, _ts) -> [] (* TODO: lookup *)
  | TypeVar id ->
      let var = TypeVarId.nth ty_params id in
      [Id var.name]
  | Bool -> [Bool]
  | Char -> [Ascii]
  | Never -> [Empty_set]
  | Integer _ -> [Z] (* TODO *)
  | Str -> [String]
  | Array t -> [Array; Space; LParen] @ toks_of_ty ty_params t @ [RParen]
  | _ -> failwith "not implemented"

let toks_of_field ty_params fld =
  match fld.field_name with
  | None -> failwith "empty field name not implemented."
  | Some name ->
      [Id name; Space; Colon; Space] @
      toks_of_ty ty_params fld.field_ty

let toks_of_field_in_record ty_params fld =
  Tab ::
  toks_of_field ty_params fld @
  [Semicolon; Newline]

let add_parens toks =
  LParen :: toks @ [RParen]

let toks_of_variant ty_params var =
  [Tab; Vert; Space; Id (var.variant_name); Space] @
  List.concat_map (fun fld -> add_parens (toks_of_field ty_params fld)) var.fields @
  [Newline]

let toks_of_type_decl (type_dec : type_decl) : token list =
  match type_dec.kind with
  | Struct fields ->
      [Record; Space; tok_of_name type_dec.name; Space] @
      toks_of_ty_vars type_dec.type_params @
      [Space; Colon; Space; Set; Space; DefEq; Space; LCurly; Newline] @
      List.concat_map (toks_of_field_in_record type_dec.type_params) fields @ 
      [Tab; RCurly; FullStop; Newline; Newline]
  | Enum variants ->
      [ Def; Space; tok_of_name type_dec.name; Space ] @
      toks_of_ty_vars type_dec.type_params @
      [Space; Colon; Space; Set; Space; DefEq; Newline] @
      List.concat_map (toks_of_variant type_dec.type_params) variants @
      [Tab; FullStop; Newline; Newline]
  | Opaque ->
      [ Axiom; Space; tok_of_name type_dec.name; Space] @
      arity_n_set_sig (List.length type_dec.type_params) @
      [FullStop; Newline; Newline]

let toks_of_raw_statement _stmt = []

let toks_of_statement stmt =
  toks_of_raw_statement stmt.content

let toks_of_opt_body obody =
  match obody with
  | None -> failwith "No body."
  | Some body -> toks_of_vars body.locals @
      [Space; DefEq; Newline] @
      toks_of_statement body.body

let toks_of_fun_decl (fun_dec : fun_decl) : token list =
  [Def; Space; tok_of_name fun_dec.name; Space] @
  toks_of_opt_body fun_dec.body @
  [FullStop; Newline; Newline]

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
