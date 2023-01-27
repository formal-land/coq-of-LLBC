open CoqOfLLBC.LLBC
open CoqOfLLBC.Parse
open CoqOfLLBC.Token
open CoqOfLLBC.Util

exception Argument_not_given of string

let () =
  let args = Sys.argv in
  let () =
    if Array.length args < 2
    then raise (Argument_not_given "Please supply an LLBC file")
    else () in
  let file = Array.get args 1 in
  match parse_crate file with
  | Ok crate ->
      let assoc = get_compact_file_assoc crate in
      let toks_assoc = toks_of_compact_assoc assoc in
      let string_assoc = assoc_list_map print_tokens toks_assoc in
      let string_assoc_with_paths = assoc_list_key_map coq_filename_of_file_name string_assoc in
      List.iter (fun ((path, file), text) ->
        let () = try_mkdirs path in
        let file_path = String.concat "/" path ^ "/" ^ file in
        output_string (open_out file_path) text) string_assoc_with_paths
  | Error msg -> print_endline msg
