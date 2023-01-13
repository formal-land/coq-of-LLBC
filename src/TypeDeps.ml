(* Dependency analysis for types *)

open Charon.Types

let ty_ids_of_type_id = function
  | AdtId i -> [i]
  | _ -> []

(* adt indices of a type *)
let rec ty_ids_of_ty = function
  | Adt(ty_id, _, tys) ->
      ty_ids_of_type_id ty_id @
      List.concat_map ty_ids_of_ty tys
  | Array t -> ty_ids_of_ty t
  | Slice t -> ty_ids_of_ty t
  | Ref(_, t, _) -> ty_ids_of_ty t
  | _ -> []

let ty_ids_of_field (f : field) =
  ty_ids_of_ty f.field_ty

let ty_ids_of_variant (v : variant) =
  List.concat_map ty_ids_of_field v.fields

let ty_ids_of_type_decl_kind = function
  | Struct fs ->
      List.concat_map ty_ids_of_field fs
  | Enum vs ->
      List.concat_map ty_ids_of_variant vs
  | Opaque -> []

let ty_ids_of_type_decl decl =
  ty_ids_of_type_decl_kind (decl.kind)

let type_deps_of_type_decl (ty_decs : type_decl list) ty_dec =
  let ids = ty_ids_of_type_decl ty_dec in
  (* remove own id in case type is recursive *)
  let ids = List.filter (fun i -> i <> ty_dec.def_id) ids in
  List.map (fun i -> List.find (fun d -> d.def_id = i) ty_decs) ids
