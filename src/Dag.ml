open Util

type 'v vertex_data = {
  in_deg : int;
  descendants : 'v list
}

(* as the name suggests, should be acyclic *)
type 'v dag = ('v, 'v vertex_data) Hashtbl.t

(* remove and return vertices with zero in-degree *)
let prune_top_elts (g : 'v dag) : ('v, 'v) compact_assoc_list =
  Hashtbl.fold (fun v vd vs ->
    if vd.in_deg = 0
    then
      let () = Hashtbl.remove g v in (v, vd.descendants) :: vs
    else vs
  ) g []

(* decrement in_deg of every element in ds *)
let decr_descendents (ds : 'v list) (g : 'v dag) : unit =
  List.iter (fun v ->
    let vd = Hashtbl.find g v in
    let vd' = { vd with in_deg = vd.in_deg - 1 } in
    Hashtbl.add g v vd'
  ) ds

(* topological sort of g using compare to break ties *)
(* see https://stackoverflow.com/a/11236027/4614475 *)
let stable_top_sort (compare : 'v -> 'v -> int)
  (g : 'v dag) : 'v list =
  let rec stable_top_sort_aux queue =
    match queue with
    | [] ->
      let top_elts = prune_top_elts g in
      let top_elts_sorted =
        List.sort (fun (v1,_) (v2,_) -> compare v1 v2) top_elts in
        (match top_elts_sorted with
        | [] -> [] (* empty or cycle *)
        | _ -> stable_top_sort_aux top_elts_sorted)
    | (v,vs) :: queue' ->
      let () = decr_descendents vs g in
      v :: stable_top_sort_aux queue'
    in stable_top_sort_aux []

(* increments the in_deg of a vertex and adds it if it doesn't yet exist *)
let incr_in_deg (g : 'v dag) (v : 'v) : unit =
  let vd' = (
    match Hashtbl.find_opt g v with
    | Some vd -> { vd with in_deg = vd.in_deg + 1 }
    | None -> { in_deg = 1; descendants = [] } ) in
  Hashtbl.replace g v vd'

(* adds descendents to a vertex, adding that vertex if necessary *)
let add_descendents (g : 'v dag) (v : 'v) (ds : 'v list) : unit =
  let vd' = (
    match Hashtbl.find_opt g v with
    | Some vd -> { vd with descendants = ds }
    | None -> { in_deg = 0; descendants = ds } ) in
  Hashtbl.replace g v vd'

(* creates a dag from pairs of vertices and their descendents *)
let dag_of_descendents (d_pairs : ('v, 'v) compact_assoc_list) : 'v dag =
  let tbl = Hashtbl.create 100 in
  let () = List.iter (fun (v, ds) ->
    let () = add_descendents tbl v ds in
    List.iter (incr_in_deg tbl) ds
    ) d_pairs in
  tbl

let dag_of_ancestors (a_pairs : ('v, 'v) compact_assoc_list) : 'v dag =
  let d_pairs = opp_compact_assoc_list a_pairs in
  dag_of_descendents d_pairs
