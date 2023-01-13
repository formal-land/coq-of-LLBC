(* Dependency analysis for globals *)
open Charon.LlbcAst
open Charon.Types

open TypeDeps

let ty_ids_of_global_decl global_dec =
  ty_ids_of_ty (global_dec.ty)

let ty_deps_of_global_decl (ty_decs : type_decl list) global_dec =
  let ids = ty_ids_of_global_decl global_dec in
  List.map (fun i -> List.find (fun d -> d.def_id = i) ty_decs) ids

let fun_deps_of_global_decl (_fun_decs : fun_decl list) _global_dec =
  [] (* TODO *)

let global_deps_of_global_decl (_global_decs : global_decl list) _global_dec =
  [] (* TODO *)
