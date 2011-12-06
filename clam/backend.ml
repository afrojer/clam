(*
 * File: backend.ml
 * Date: 2011-10-17
 *
 * PLT Fall 2011
 * CLAM Project
 * Jeremy C. Andrus <jeremya@cs.columbia.edu>
 * Robert Martin <rdm2128@columbia.edu>
 * Kevin Sun <kfs2110@columbia.edu>
 * Yongxu Zhang <yz2419@columbia.edu>
 *
 *)

open String
open Clamtypes
open Ast

(*
 * C placeholders and identifiers
 *)
let c_ident_of_ImgT imgT =
  "_Img_" ^ imgT.iname


(*
 * C type definitions
 *)
let c_def_ImgT =
  "typedef struct {\n" ^
  "  char *iname;\n" ^
  "} _ImageT;\n"


(*
 * C variable declarations
 *)
let c_decl_of_ImgT imgT =
  "_ImageT " ^ (c_ident_of_ImgT imgT) ^ " = {\n" ^
  "  .iname = \"" ^ (escaped imgT.iname) ^ "\"\n" ^
  "};\n"


(*
 * C Code Generation
 *)

let c_of_vdecl = function
    ImageT(id) -> "/* Declare ImageT "^id^" */"
  | KernelT(id) -> "/* Declare KernelT "^id^" */"
  | CalcT(id,atom) -> "/* Declare CalcT "^id^" of type "^(Printer.string_of_type atom)^" */"

let c_of_assign op e =
  "/* Assignment */"

let c_of_libf libf elist =
   "/* Libf */"

let c_of_expr = function
    LibCall(libf,elist) -> (c_of_libf libf elist)
  | _ -> "/* Unknown Expression */"

let c_of_stmt stmt =
  let c_stmt = match stmt with
      Expr(e) -> (c_of_expr e)
    | VDecl(v) -> (c_of_vdecl v)
    | VAssign(v,op,e) -> (c_of_vdecl v) ^ (c_of_assign op e)
  in
  c_stmt ^ ";\n"




(*
 * Splicing Code Together
 *)
let generate_preamble_c env ast =
  "#include <stdio.h>\n" ^
  c_def_ImgT

let generate_definitions_c env ast =
  (List.fold_left (^) "" (List.map c_decl_of_ImgT env.images))

let generate_main_c env ast =
  "int main(int argc, char *argv) {\n" ^
  (List.fold_left (^) "" (List.map c_of_stmt ast)) ^
  "  return 0;\n" ^
  "}\n"

let generate_c env ast =
  let c_source =
    "\n/* CLAM: PREAMBLE */\n" ^
    (generate_preamble_c env ast) ^
    "\n/* CLAM: DEFINITIONS */\n" ^
    (generate_definitions_c env ast) ^
    "\n/* CLAM: MAIN */\n" ^
    (generate_main_c env ast)
  in
  c_source

