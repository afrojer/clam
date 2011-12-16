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
open Envtypes
open Ast
open Sast


(*
 * Variable Declarations
 *)
let c_of_imgDecl imgT =
  "/* Declare ImageIdent: " ^ imgT.iname ^ " */\n"

let c_of_kernDecl kernT =
  "/* Declare KernelIdent: " ^ kernT.kname ^ " */\n"

let c_of_calcDecl calcT =
  "/* Declare CalcIdent: " ^ calcT.cname ^ " */\n"

(*
 * Variable Definitions
 *)

(* TODO: Initialize the Calc variables *)


(*
 * Main C Functions
 *)
let rec c_of_calcAssign ca =
  let c_of_rhs = c_of_calcEx ca.c_rhs in
    "/* Calc Assignment: Prepare RHS */\n" ^
    (c_of_rhs) ^
    "/* Calc Assignment: Store in: " ^ ca.c_lhs.cid ^ "*/\n"

and c_of_calcEx ce = match ce with
    CMatrix(m) -> "/* C Matrix */\n"
  | CRaw(s,ids) -> "/* C String: '" ^ s ^ "' */\n"
  | CChain(ca) -> c_of_calcAssign ca
  | CIdent(id) -> "/* C Calc ID: " ^ id.cid ^ " */\n"

let c_of_kernEx ke =
  "/* C of Kernel Expression */\n"

let c_of_imgEx ie =
  "/* C of Image Expression */\n"

let c_of_chanRefEx che =
  "/* C of ChanRef Expression */\n"

let c_of_imgWrite ie fmt fid =
  "/* C of ImgWrite Expression */\n"

let c_of_scope scope =
  let venv = scope.venv in
    (List.fold_left (^) "" (List.map c_of_imgDecl venv.images)) ^
    (List.fold_left (^) "" (List.map c_of_kernDecl venv.kernels)) ^
    (List.fold_left (^) "" (List.map c_of_calcDecl venv.calc))

let c_of_vExpr = function
    Debug(s) -> "/* DEBUG: " ^ s ^ " */\n"
  | CalcEx(ce) -> c_of_calcEx ce
  | KernelEx(ke) -> c_of_kernEx ke
  | ImageEx(ie) -> c_of_imgEx ie
  | ChanRefEx(che) -> c_of_chanRefEx che
  | ImgWriteEx(ie,fmt,fid) -> c_of_imgWrite ie fmt fid

let generate_c scope vast =
  "\n/* GENERATED ENVIRONMENT C */\n" ^
  (c_of_scope scope) ^
  "\n/* GENERATED MAIN C */\n" ^
  "int main(int argc, char **argv) {\n" ^
  (List.fold_left (^) "" (List.map c_of_vExpr vast)) ^
  "  return 0;\n" ^
  "}\n"

