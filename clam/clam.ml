(*
 * File: clam.ml
 * Date: 2011-10-16
 *
 * PLT Fall 2011
 * CLAM Project
 * Jeremy C. Andrus <jeremya@cs.columbia.edu>
 * Robert Martin <rdm2128@columbia.edu>
 * Kevin Sun <kfs2110@columbia.edu>
 * Yongxu Zhang <yz2419@columbia.edu>
 *
 *)

let clambin_out = "bin.clam"

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    let program = Parser.expr Scanner.token lexbuf in
    let c_code = Backend.generate_c program in
    Clamsys.compile_c c_code clambin_out; exit 0
  with
      Failure(s)           -> prerr_endline ("Error: "^s); exit 1
    | Parsing.Parse_error  -> prerr_endline ("Syntax error"); exit 1
    | Sys_error(s)         -> prerr_endline ("System error - check permissions on '"^
                                             Filename.temp_dir_name^"': "^s); exit 1
    | Clamsys.Compile_error(s) -> prerr_endline ("C-Backend error: "^s); exit 1

