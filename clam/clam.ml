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

open Clamtypes

let clamversion = "0.1"
let clam_binout = ref "bin.clam"
let clam_srcin  = ref "-"

let clam_usage =
  let s1 = "CLAM v"^clamversion^"\n" in
  let s2 = Printf.sprintf "Usage: %s [options] {{<} inputfile}\n"
           (Filename.basename Sys.executable_name) in
  let s3 = "Options are:" in
  s1 ^ s2 ^ s3

let set_clam_output s =
  clam_binout := s

let set_clam_input s =
  clam_srcin := s

let clam_anon_fcn = function
  | "-" -> clam_srcin := "-"
  | filename -> clam_srcin := filename

let _ =
  let args =
    [  "-o", Arg.String set_clam_output, "<filename> Specify the output file";
       "-i", Arg.String set_clam_input, "<filename> Specify the input file";
    ] in
  Arg.parse (Arg.align args) clam_anon_fcn clam_usage;
  try
    let program = if clam_srcin = ref "-" then
                  Parse_util.parse_stdin () else
                  Parse_util.parse_file !clam_srcin in
    let (env, verified_prog) = Verifier.verify program in
    let c_code = Backend.generate_c env verified_prog in
    Clamsys.compile_c c_code !clam_binout; exit 0
  with
      Failure(s)           -> prerr_endline ("Error: "^s); exit 1
    | ParseErr(e,s) as err -> print_clamerr err; exit 1
    | Sys_error(s)         -> prerr_endline 
                              ("System error - check permissions on '"^
                                Filename.temp_dir_name^"': "^s); exit 1
    | Clamsys.Compile_error(s) -> prerr_endline ("C-Backend error: "^s); exit 1

