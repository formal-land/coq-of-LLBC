(* Dependency analysis for functions *)
open Charon.LlbcAst
open Charon.Types

open TypeDeps

let ty_ids_of_fun_sig fsig =
  let input_ids = List.concat_map ty_ids_of_ty fsig.inputs in
  let output_ids = ty_ids_of_ty fsig.output in
  input_ids @ output_ids

let ty_ids_of_fun_decl fun_dec =
  ty_ids_of_fun_sig (fun_dec.signature)

let ty_deps_of_fun_decl (ty_decs : type_decl list) fun_dec =
  let ids = ty_ids_of_fun_decl fun_dec in
  List.map (fun i -> List.find (fun d -> d.def_id = i) ty_decs) ids

let fun_deps_of_fun_decl (_fun_decs : fun_decl list) _fun_dec =
  [] (* TODO *)

let global_deps_of_fun_decl (_global_decs : global_decl list) _fun_dec =
  [] (* TODO *)
