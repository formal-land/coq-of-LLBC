open Charon.Meta
open Charon.Names

open CoqOfLLBC.LLBC
open CoqOfLLBC.Parse
open CoqOfLLBC.Util

(* some debugging code for now *)
let print_compact_assoc_list (print_a : 'a -> unit) (print_b : 'b -> unit)
  (ps : ('a, 'b) compact_assoc_list) : unit =
  List.iter (fun (a,bs) ->
    let () = print_a a in
    List.iter print_b bs
  ) ps

let print_file_name (name : file_name) : unit =
  let str = show_file_name name ^ ":" in
  print_endline str

let print_def (def : definition) : unit =
  let def_name = show_name (name_of_def def) in
  let str = (
    match def with
    | Type_def _ -> "\t\tType: " ^ def_name
    | Fun_def _ -> "\t\tFunction: " ^ def_name
    | Global_def _ -> "\t\tGlobal: " ^ def_name
  ) in
  print_endline str

let () =
  let file = "test/tests.llbc" in (* TODO: get from args *)
  match parse_crate file with
  | Ok crate ->
      let assoc = get_top_sorted_compact_file_assoc crate in
      print_compact_assoc_list print_file_name print_def assoc
  | Error msg -> print_endline msg
