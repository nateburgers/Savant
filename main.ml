open Core.Std
module ANF = Administrative_normal_form

(* Entry Point for the Compiler *)

let command =
  Command.basic
    ~summary: "Savant"
    Command.Spec.(
    empty
    +> flag "-debug" no_arg ~doc: "boolean start debugging session"
    +> flag "-o" (required string) ~doc: "string output file name"
  )
    (fun debug output_file_name () ->
     print_string output_file_name
    )
let () =
  Command.run ~version: "0.0.1" ~build_info:"RWO" command
