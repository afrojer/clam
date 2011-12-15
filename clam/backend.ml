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
open Environ
open Ast

(*
 * C placeholders and identifiers
 *)
let c_ident_of_ImgT imgT =
  "_Img_" ^ imgT.iname






(* NOTE: We are not required to declare Calc, Kernel, or Image
 * in the same place that the CLAM programmer did it. This
 * section can do nothing as long as these variables are
 * listed in the 'env' variable and can therefore be
 * declared as global variables in the C source...         *)
let c_of_vdecl v = match v with
    ImageT(id)     -> "/* Declare ImageT: "^id^", */\n"
  | KernelT(id)    -> "/* Declare KernelT "^id^" */\n"
  | CalcT(id,atom) -> "/* Declare CalcT "^id^" of type "^(Printer.string_of_atom atom)^" */\n"
  | ConvT(_,_) | KCalcT(_) | StrT(_,_) | BareT(_) ->
        raise (Failure("Internal error [internal type used in vdecl)"))

let c_of_assign op e =
  "printf(\"Assignment operation\\n\");\n"

let c_of_libf libf elist =
   "printf(\"Libf Call\\n\");\n"

let c_of_expr expr = match expr with
    LibCall(libf,elist) -> (c_of_libf libf elist)
(* TODO: Lots more expressions to match with here! *)
  | _ -> "printf(\"Expression\\n\");\n"

let c_of_stmt stmt =
  match stmt with
      Expr(e) -> (c_of_expr e)
    | VDecl(v) -> (c_of_vdecl v)
    | VAssign(v,op,e) -> (c_of_vdecl v) ^ (c_of_assign op e)




(*
 * Splicing Code Together
 *)

let generate_preamble_c =
  "#include <stdio.h>\n" ^
  "/* Dump the contents of our clam.h file here */\n"

let generate_declarations_c env =
  "/* Generate declarations for our variables here, using the environment\n" ^
  " * variable (env). This will probable include:\n" ^
  " *   - Calc definition (using #define)\n" ^
  " *   - Global variables corresponding to ImageT pointers\n" ^
  " *   - Global variables corresponding to KernelT pointers\n"

let generate_main_c ast =
  "/* Strategy: Map each statement in the AST to its corresponding C source code.\n" ^
  " *           Then, append this text together (using ^ aka strcat).\n" ^
  " */\n" ^
  (List.fold_left (^) "" (List.map c_of_stmt ast))
  

let generate_c env ast =
  let c_source =
    "\n/* CLAM: PREAMBLE */\n" ^
    (generate_preamble_c)  ^
    "\n/* CLAM: DEFINITIONS */\n" ^
    (generate_declarations_c env) ^
    "\n/* CLAM: MAIN */\n" ^
    "int main(int argc, char *argv) {\n" ^
    (generate_main_c ast) ^
    "\n" ^
    "  return 0;\n" ^
    "}\n"
  in
  c_source

