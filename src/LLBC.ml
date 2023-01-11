open Charon.LlbcAst
open Charon.LlbcOfJson
open Charon.Meta
open Charon.Types

open Util

let crate_of_file file_name =
  let json = Yojson.Basic.from_file file_name in
  crate_of_json json

type definition =
  | Type_def of type_decl
  | Fun_def of fun_decl
  | Global_def of global_decl

let get_dependencies (_ : definition) : definition list =
  failwith "TODO: implement get_dependencies"

let crate_defs crate =
  List.map (fun t -> Type_def t) (crate.types) @
  List.map (fun f -> Fun_def f) (crate.functions) @
  List.map (fun g -> Global_def g) (crate.globals)

let file_name_of_def = function
  | Type_def t -> t.meta.span.file
  | Fun_def f -> f.meta.span.file
  | Global_def g -> g.meta.span.file

let beg_line_of_def = function
  | Type_def t -> t.meta.span.beg_loc.line
  | Fun_def f -> f.meta.span.beg_loc.line
  | Global_def g -> g.meta.span.beg_loc.line

(* used for debugging
let name_of_def = function
  | Type_def t -> t.name
  | Fun_def f -> f.name
  | Global_def g -> g.name
*)

let compare_by_beg_line def_1 def_2 =
  let line_1 = beg_line_of_def def_1 in
  let line_2 = beg_line_of_def def_2 in
  Int.compare line_1 line_2

let get_file_assoc crate =
  List.map (fun d -> (file_name_of_def d, d)) (crate_defs crate)

let get_compact_file_assoc crate =
  let file_defs = get_file_assoc crate in
  compact_assoc_list_of_assoc_list file_defs

let dag_of_file_defs (file, defs) =
  let def_deps = List.map (fun d -> d, get_dependencies d) defs in
  let dag = Dag.dag_of_ancestors def_deps in
  (file, dag)

let get_top_sorted_compact_file_assoc crate : (file_name, definition) compact_assoc_list =
  let file_defs = get_compact_file_assoc crate in
  let file_dags = List.map dag_of_file_defs file_defs in
  let file_defs = List.map (fun (file, dag) ->
    (file, Dag.stable_top_sort compare_by_beg_line dag)) file_dags in
  file_defs
