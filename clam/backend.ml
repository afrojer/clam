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
let c_of_kernCalc ids =
  "/* Kernel of Calcs: " ^ (List.fold_left (^) "" (List.map ((^) " ") ids)) ^ " */\n"

let rec c_of_calcEx = function
    CMatrix(m) -> "/* C Matrix */\n"
  | CRaw(s,ids) -> "/* C String: '" ^ s ^ "' */\n"
  | CChain(ca) -> c_of_calcAssign ca
  | CIdent(id) -> "/* C Calc ID: " ^ id ^ " */\n"

and c_of_calcAssign ca =
  let c_of_rhs = c_of_calcEx ca.c_rhs in
    "/* --> Calc Assignment: Prepare RHS */\n" ^
    (c_of_rhs) ^
    "/* <-- Calc Assignment: Store in: " ^ ca.c_lhs ^ " */\n"

let rec c_of_kernEx = function
    KCalcList(ids) -> c_of_kernCalc ids
  | KChain(ka) -> c_of_kernAssign ka
  | KAppend(kap) -> c_of_kernAppend kap
  | KIdent(id) -> "/* C Kernel ID: " ^ id ^ " */\n"

and c_of_kernAssign ka =
  let c_of_rhs = c_of_kernEx ka.k_rhs in
    "/* --> Kern Assignment: Prepare RHS */\n" ^
    c_of_rhs ^
    "/* <-- Kern Assignment: Store in: " ^ ka.k_lhs ^ " */\n"

and c_of_kernAppend kap =
  let c_of_rhs = c_of_calcEx kap.ka_rhs in
    "/* --> Kern Append: Prepare RHS */\n" ^
    c_of_rhs ^
    "/* <-- Kern Appen: Append to: " ^ kap.ka_lhs ^ " */\n"

let c_of_conv cid ke =
  let c_of_rhs = c_of_kernEx ke in
    "/* --> Convolve: Prepare Kernel */\n" ^
    c_of_rhs ^
    "/* <-- Convolve: ??? */\n"

let c_of_imgread fid =
  match fid with
      Const(s) -> "/* filename: " ^ s ^ " */\n"
    | Arg(i) -> "/* filename: Arg " ^ (string_of_int i) ^ " */\n"

let rec c_of_imAssign ia =
  "/* --> Image Assign: Prepare RHS */\n" ^
  (c_of_imgEx ia.i_rhs) ^
  "/* <-- Image Assign: Assign to " ^ ia.i_lhs ^ " */\n"

and c_of_imAppend iap =
  "/* --> Image Append: Preapre RHS */\n" ^
  (c_of_calcEx iap.ia_rhs) ^
  "/* <-- Image Append: Append to " ^ iap.ia_lhs ^ " */\n"

and c_of_imgEx = function
    ImConv(cid,ke) -> c_of_conv cid ke
  | ImRead(fid) -> c_of_imgread fid
  | ImChain(ia) -> c_of_imAssign ia
  | ImAppend(iap) -> c_of_imAppend iap
  | ImIdent(id) -> "/* C Image ID: " ^ id ^ " */\n"

let rec c_of_chanRefEx = function
    ChanChain(ca) -> c_of_chanAssign ca
  | ChanIdent(cid) -> "/* C Chan Ref: " ^ fst(cid) ^ ":" ^ snd(cid) ^ " */\n"

and c_of_chanAssign ca =
  "/* --> ChanAssign: Prepare RHS */\n" ^
  (c_of_chanRefEx ca.ch_rhs) ^
  "/* <-- ChanAssign: Store in " ^ fst(ca.ch_lhs) ^ ":" ^ snd(ca.ch_lhs) ^ " */\n"

let c_of_imgWrite ie fmt fid =
  "/* --> ImgWrite: Prepare Image */\n" ^
  (c_of_imgEx ie) ^
  "/* <-- C of ImgWrite Expression */\n"

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

