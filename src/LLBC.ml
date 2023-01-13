open Charon.LlbcAst
open Charon.Meta
open Charon.Types

open FunDeps
open GlobalDeps
open TypeDeps
open Util

type definition =
  | Type_def of type_decl
  | Fun_def of fun_decl
  | Global_def of global_decl

let type_def t = Type_def t

let fun_def f = Fun_def f

let global_def g = Global_def g

let get_dependencies def crate : definition list =
  match def with
  | Type_def ty_dec ->
      List.map type_def (type_deps_of_type_decl crate.types ty_dec)
  | Fun_def fun_dec ->
      List.map type_def (ty_deps_of_fun_decl crate.types fun_dec) @
      List.map fun_def (fun_deps_of_fun_decl crate.functions fun_dec) @
      List.map global_def (global_deps_of_fun_decl crate.globals fun_dec)
  | Global_def global_dec ->
      List.map type_def (ty_deps_of_global_decl crate.types global_dec) @
      List.map fun_def (fun_deps_of_global_decl crate.functions global_dec) @
      List.map global_def (global_deps_of_global_decl crate.globals global_dec)

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

(* used for debugging *)
let name_of_def = function
  | Type_def t -> t.name
  | Fun_def f -> f.name
  | Global_def g -> g.name

let compare_by_beg_line def_1 def_2 =
  let line_1 = beg_line_of_def def_1 in
  let line_2 = beg_line_of_def def_2 in
  Int.compare line_1 line_2

let get_file_assoc crate =
  List.map (fun d -> (file_name_of_def d, d)) (crate_defs crate)

let get_compact_file_assoc crate =
  let file_defs = get_file_assoc crate in
  compact_assoc_list_of_assoc_list file_defs

let dag_of_file_defs crate (file, defs) =
  let def_deps = List.map (fun d -> d, get_dependencies d crate) defs in
  let dag = Dag.dag_of_ancestors def_deps in
  (file, dag)

let get_top_sorted_compact_file_assoc crate : (file_name, definition) compact_assoc_list =
  let file_defs = get_compact_file_assoc crate in
  let file_dags = List.map (dag_of_file_defs crate) file_defs in
  let file_defs = List.map (fun (file, dag) ->
    (file, Dag.stable_top_sort compare_by_beg_line dag)) file_dags in
  file_defs
