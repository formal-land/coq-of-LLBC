let pair a b = (a,b)

let swap (a,b) = (b,a)

let cons x l = x :: l

let rec last xs =
  match xs with
  | [] -> failwith "empty list"
  | [x] -> x
  | _ :: xs -> last xs

let rec intersperse sep xs =
  match xs with
  | [] | [_] -> xs
  | x :: xs -> x :: sep :: intersperse sep xs

let rec ne_prefixes xs =
  match xs with
  | [] -> []
  | x :: ys -> [x] :: List.map (cons x) (ne_prefixes ys)

type ('a, 'b) assoc_list = ('a * 'b) list

type ('a, 'b) compact_assoc_list = ('a * 'b list) list

let assoc_list_key_map (f : 'a -> 'b) (ps : ('a, 'c) assoc_list) : ('b, 'c) assoc_list =
  List.map (fun (k,v) -> (f k, v)) ps

let assoc_list_map (f : 'b -> 'c) (ps : ('a, 'b) assoc_list) : ('a, 'c) assoc_list =
  List.map (fun (k,v) -> (k, f v)) ps

let compact_assoc_list_map (f : 'b -> 'c) (ps : ('a, 'b) compact_assoc_list) : ('a, 'c) compact_assoc_list =
  List.map (fun (k,vs) -> (k, List.map f vs)) ps

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

let concat_compact_assoc_list (ps : ('a, 'b list) compact_assoc_list) : ('a, 'b list) assoc_list =
  List.map (fun (k,ls) -> (k, List.concat ls)) ps