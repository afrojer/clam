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
 * Identifier Translations
 *)
let id_of_imgId imgId = "__imgT_" ^ imgId
let id_of_kernId kernId = "__kernT_" ^ kernId
let id_of_calcId calcId = "__calcT_" ^ calcId

let id_of_imgT imgT = id_of_imgId imgT.iname
let id_of_kernT kernT = id_of_kernId kernT.kname
let id_of_calcT calcT = id_of_calcId calcT.cname
let id_of_chanT chanT = "clam_imgchan_ref( " ^ (id_of_imgId(fst chanT)) ^ ", \"" ^ (escaped(snd chanT)) ^ "\")"

(*
 * Variable Declarations
 *)
let c_of_imgDecl imgT = "clam_img *" ^ (id_of_imgT imgT) ^ " = NULL;\n"
let c_of_kernDecl kernT = "clam_kernel *" ^ (id_of_kernT kernT) ^ " = NULL;\n"
let c_of_calcDecl calcT = "clam_calc *" ^ (id_of_calcT calcT) ^ " = NULL;\n"



(*
 * Variable Definitions
 *)

(* TODO: Initialize the Calc variables? *)


(*
 * Main C Functions
 *)

let c_of_fmt = function
    Png -> "PNG"
  | Bmp -> "BMP"
  | Tga -> "TGA"

let c_of_fid = function
    Arg(i) -> "argv[" ^ (string_of_int i) ^ "]"
  | Const(s) -> "\"" ^ (escaped s) ^ "\""

let c_of_matrix m =
  let ( (wid, hei), (num, den), rowCol ) = m in
  (string_of_int wid) ^ ", " ^ (string_of_int hei) ^ ", " ^
  (string_of_int num) ^ ", " ^ (string_of_int den) ^ ", " ^
  let c_of_matrix_row int_row =
    let str_row = List.map string_of_int int_row in
    let rs = (List.hd str_row) :: (List.map ((^) ", ") (List.tl str_row)) in
    "{" ^ (List.fold_left (^) "" rs) ^ "}"
  in
  let raw_rows = List.map c_of_matrix_row rowCol in
  let rows = (List.hd raw_rows) :: (List.map ((^) ", ") (List.tl raw_rows)) in
  "{ " ^ (List.fold_left (^) "" rows) ^ " }"
  

let c_of_kernCalc ids =
  "/* XXX: c_of_kernCalc should be an r-value by building up a kernel in memory, but possible memory leak? */"
(*
  "/* Kernel of Calcs: " ^ (List.fold_left (^) "" (List.map ((^) " ") ids)) ^ " */\n"
*)


let rec c_of_calcEx = function
    CMatrix(m) -> "/* TODO: Read-Only Matrix: " ^ (c_of_matrix m) ^ " */"
  | CRaw(s,ids) -> "/* TODO: Read-Only C String: '" ^ s ^ "' */"
  | CChain(ca) -> c_of_calcAssign ca
  | CIdent(id) -> id_of_calcId id

and c_of_calcAssign ca =
  (id_of_calcId ca.c_lhs) ^ " = " ^ (c_of_calcEx ca.c_rhs)

let rec c_of_kernEx = function
    KCalcList(ids) -> c_of_kernCalc ids
  | KChain(ka) -> c_of_kernAssign ka
  | KAppend(kap) -> c_of_kernAppend kap
  | KIdent(id) -> id_of_kernId id

and c_of_kernAssign ka =
  "clam_kernel_copy(" ^ (id_of_kernId ka.k_lhs) ^ ", (" ^ (c_of_kernEx ka.k_rhs) ^ ") )"

and c_of_kernAppend kap =
  "clam_kernel_addcalc(" ^ (id_of_kernId kap.ka_lhs) ^ ", (" ^ (c_of_calcEx kap.ka_rhs) ^ ") )"

let c_of_conv cid ke =
  "/* XXX: CONVOLUTION */"
(*
  let c_of_rhs = c_of_kernEx ke in
    "/* --> Convolve: Prepare Kernel */\n" ^
    c_of_rhs ^
    "/* <-- Convolve: ??? */\n"
*)

let c_of_imgread fid =
  "imgread(" ^ (c_of_fid fid) ^ ")"

let rec c_of_imAssign ia =
  let img_needs_cloning = function
      ImConv(cid,ke) -> false
    | ImRead(fid)    -> false
    | ImChain(ia)    -> true
    | ImAppend(iaP)  -> true
    | ImIdent(id)    -> true
  in
  let c_rhs =
    if (img_needs_cloning ia.i_rhs)
    then "clam_img_copy( (" ^ (c_of_imgEx ia.i_rhs) ^ ") )"
    else c_of_imgEx ia.i_rhs
  in
  "clam_img_assign(" ^ (id_of_imgId ia.i_lhs) ^ ", " ^ c_rhs ^ ")"

and c_of_imAppend iap =
  "clam_imgchan_addcalc(" ^ (id_of_imgId iap.ia_lhs) ^ ", (" ^ (c_of_calcEx iap.ia_rhs) ^ ") )"

and c_of_imgEx = function
    ImConv(cid,ke) -> c_of_conv cid ke
  | ImRead(fid) -> c_of_imgread fid
  | ImChain(ia) -> c_of_imAssign ia
  | ImAppend(iap) -> c_of_imAppend iap
  | ImIdent(id) -> id_of_imgId id

let rec c_of_chanRefEx = function
    ChanChain(ca) -> c_of_chanAssign ca
  | ChanIdent(cid) -> id_of_chanT cid

and c_of_chanAssign ca =
  "clam_imgchan_copy( " ^
  (id_of_imgId(fst ca.ch_lhs)) ^
  ", " ^
  "\"" ^ (escaped(snd ca.ch_lhs)) ^ "\"" ^
  ", " ^
  (c_of_chanRefEx ca.ch_rhs) ^ ")"

let c_of_imgWrite ie fmt fid =
  "imgwrite( (" ^ (c_of_imgEx ie) ^ ") , " ^ (c_of_fmt fmt) ^ " , " ^ (c_of_fid fid) ^ " )"



(*
 * Glue
 *)
let c_of_scope scope =
  let venv = scope.venv in
    (List.fold_left (^) "" (List.map c_of_imgDecl venv.images)) ^
    (List.fold_left (^) "" (List.map c_of_kernDecl venv.kernels)) ^
    (List.fold_left (^) "" (List.map c_of_calcDecl venv.calc))

let c_of_vExpr = function
    Debug(s) -> "/* DEBUG: " ^ s ^ " */"
  | CalcEx(ce) -> c_of_calcEx ce
  | KernelEx(ke) -> c_of_kernEx ke
  | ImageEx(ie) -> c_of_imgEx ie
  | ChanRefEx(che) -> c_of_chanRefEx che
  | ImgWriteEx(ie,fmt,fid) -> c_of_imgWrite ie fmt fid

let c_of_vStmt vExpr =
  "  " ^ (c_of_vExpr vExpr) ^ ";\n"

let generate_c scope vast =
  "\n/* GENERATED HEADER C */\n" ^
  Clam_clib.clibheader ^
  "\n/* GENERATED ENVIRONMENT C */\n" ^
  (c_of_scope scope) ^
  "\n/* GENERATED MAIN C */\n" ^
  "int main(int argc, char **argv) {\n" ^
  let m = string_of_int scope.max_arg in
  "  if (argc <= " ^ m ^ ") {\n" ^
  "    fprintf(stderr, \"This program requires " ^ m ^ " arguments.\\n\");\n" ^
  "    exit(1);\n" ^
  "  }\n" ^
  (List.fold_left (^) "" (List.map c_of_vStmt vast)) ^
  "\n  return 0;\n" ^
  "}\n"

