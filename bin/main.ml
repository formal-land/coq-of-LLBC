(*open Charon.Meta

open CoqOfLLBC.Util

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
  let def_name = 
*)

let () =
  let msg = (
  match CoqOfLLBC.LLBC.crate_of_file "test/tests.llbc" with
  | Ok crate -> Charon.LlbcAst.show_gcrate (fun _ _ -> ()) (fun _ _ -> ()) crate
  | Error msg -> msg
  ) in
  print_endline msg