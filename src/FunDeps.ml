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

let fun_ids_of_fun_id = function
  | Regular i -> [i]
  | Assumed _ -> []

let fun_ids_of_call call =
  fun_ids_of_fun_id call.func

let rec fun_ids_of_raw_statement = function
  | Call call -> fun_ids_of_call call
  | Sequence (stmt1, stmt2) ->
      fun_ids_of_statement stmt1 @
      fun_ids_of_statement stmt2
  | Loop stmt ->
      fun_ids_of_statement stmt
  | _ -> []

and fun_ids_of_statement stmt =
  fun_ids_of_raw_statement stmt.content

let fun_ids_of_gexpr_body (ge_body : _ gexpr_body) =
  fun_ids_of_statement ge_body.body

let fun_ids_of_fun_decl (fun_dec : fun_decl) =
  match fun_dec.body with
  | None -> []
  | Some ge_body -> fun_ids_of_gexpr_body ge_body

let fun_deps_of_fun_decl (fun_decs : fun_decl list) (fun_dec : fun_decl) =
  let ids = fun_ids_of_fun_decl fun_dec in
  (* remove own id in case function is recursive *)
  let ids = List.filter (fun i -> i <> fun_dec.def_id) ids in
  List.map (fun i -> List.find (fun (f : _ gfun_decl) -> f.def_id = i) fun_decs) ids

let global_deps_of_fun_decl (_global_decs : global_decl list) _fun_dec =
  [] (* TODO *)
