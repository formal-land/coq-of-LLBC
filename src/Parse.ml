open Charon.LlbcOfJson

let parse_crate file_name =
  let json = Yojson.Basic.from_file file_name in
  crate_of_json json
