let pair a b = (a,b)

let swap (a,b) = (b,a)

type ('a, 'b) assoc_list = ('a * 'b) list

type ('a, 'b) compact_assoc_list = ('a * 'b list) list

let assoc_list_of_compact_assoc_list
  (ps : ('a, 'b) compact_assoc_list) : ('a, 'b) assoc_list =
  List.concat_map (fun (a, bs) -> List.map (pair a) bs) ps

let opp_assoc_list (ps : ('a, 'b) assoc_list) : ('b, 'a) assoc_list =
  List.map swap ps

let rec compact_assoc_list_insert (ps : ('a, 'b) compact_assoc_list)
  (k : 'a) (v : 'b) : ('a, 'b) compact_assoc_list =
  match ps with
  | [] -> [(k, [v])]
  | (k', vs) :: tl ->
      if k = k'
      then (k', v :: vs) :: tl
      else (k', vs) :: compact_assoc_list_insert tl k v

let compact_assoc_list_of_assoc_list
  (ps : ('a, 'b) assoc_list) : ('a, 'b) compact_assoc_list =
  List.fold_right (fun (k,v) qs -> compact_assoc_list_insert qs k v) ps []

let opp_compact_assoc_list (ps : ('a, 'b) compact_assoc_list) : ('b, 'a) compact_assoc_list =
  let ps = assoc_list_of_compact_assoc_list ps in
  let ps = opp_assoc_list ps in
  compact_assoc_list_of_assoc_list ps
