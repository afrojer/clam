(*
 * File: clam.ml
 * Date: 2011-10-16
 *
 * PLT Fall 2011
 * CLAM Project
 * Jeremy C. Andrus <jeremya@cs.columbia.edu>
 * Robert Martin <rdm2128@columbia.edu>
 * Kevin Sun <kfs2110@columbia.edu> * Yongxu Zhang <yz2419@columbia.edu>
 *
 *)


let clamversion = "0.1"
let clam_binout = ref "a.out"
let clam_c_out  = ref "clam_gen.c"
let clam_c_only = ref false
let clam_print_ast = ref false
let clam_srcin  = ref "-"

let clam_usage =
  let s1 = "CLAM v"^clamversion^"\n" in
  let s2 = Printf.sprintf "Usage: %s [options] {{<} inputfile}\n"
           (Filename.basename Sys.executable_name) in
  let s3 = "Options are:" in
  s1 ^ s2 ^ s3

let set_clam_output s =
  clam_binout := s;
  clam_c_out := s

let set_clam_input s =
  clam_srcin := s

let set_clam_gen_c_only () =
  clam_c_only := true

let set_clam_print_ast () =
  clam_print_ast := true

let clam_anon_fcn = function
  | "-" -> clam_srcin := "-"
  | filename -> clam_srcin := filename

let _ =
  let args =
    [  "-o", Arg.String set_clam_output, "<filename> Specify the output file";
       "-i", Arg.String set_clam_input, "<filename> Specify the input file";
       "-c", Arg.Unit set_clam_gen_c_only, " Output generated C only";
       "-t", Arg.Unit set_clam_print_ast, " Print AST debugging information";
    ] in
  Arg.parse (Arg.align args) clam_anon_fcn clam_usage;
  try
    let input_prog = if clam_srcin = ref "-" then
                       Parseutil.parse_stdin ()
                     else
                       Parseutil.parse_file !clam_srcin in
    let program = List.rev input_prog in
    (* print out the AST if requested *)
    let _ = if !clam_print_ast then
              print_endline (Printer.string_of_ast program) else () in
    let (env, verified_ast) = Verifier.verify program in
    let (scope, sast) = Semantic.translate_ast env verified_ast in
    let c_code = Backend.generate_c scope sast in
    if !clam_c_only then
      let ochan = Pervasives.open_out !clam_c_out in
      let _ = Pervasives.output_string ochan c_code in
      let _ = Pervasives.close_out ochan in
      exit 0
    else
      Clamsys.compile_c c_code !clam_binout; exit 0
  with
      Failure(s)           -> prerr_endline ("Error: "^s); exit 1
    | Semantic.SemanticFailure(s) -> prerr_endline ("Semantic Error: "^s); exit 1
    | Parseutil.ParseErr(e,s) as err -> Printer.print_clamerr err; exit 1
    | Sys_error(s)         -> prerr_endline 
                              ("System error - check permissions on '"^
                                Filename.temp_dir_name^"': "^s); exit 1
    | Clamsys.Compile_error(s) -> prerr_endline ("C-Backend error: "^s); exit 1

