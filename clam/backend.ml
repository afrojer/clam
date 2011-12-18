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

let c_of_atom = function
    Uint8  -> ("UINT8", "uint8_t")
  | Uint16 -> ("UINT16", "uint16_t")
  | Uint32 -> ("UINT32", "uint32_t")
  | Int8   -> ("INT8", "int8_t")
  | Int16  -> ("INT16", "int16_t")
  | Int32  -> ("INT32", "int32_t")
  | Angle  -> ("ANGLE", "float")
  | _ -> raise(Failure("Backend finding the Calc type of Unknown or Angle?"))

let c_of_fmt = function
    Png -> "PNG"
  | Bmp -> "BMP"
  | Tga -> "TGA"


let inner_c_of_cfunc_calc calclst ct =
  let cinfo_from_id id = (List.fold_left (fun (ii,idx,ct) c ->
                                         if (c.cname = id) then (ii+1,ii,c) else (ii+1,idx,ct))
                         (0,0,ct)
                         calclst) in
  let pixparms ct idx = (snd (c_of_atom ct.ctype))^","^(string_of_int idx) in
  let cdef_of_cfunc_id id =
    let _,idx,idcalc = cinfo_from_id id in
    "\t\t#define "^id^"  clam_img_pix("^(pixparms idcalc idx)^")\n" in
  let idlist = (snd ct.cfunc) in
  (List.fold_left (^) "" (List.map cdef_of_cfunc_id idlist)) ^
  "\t\t#define cfunc ("^(fst ct.cfunc)^")\n"^
  "\t\tclam_convolve_cfunc("^(id_of_calcId ct.cname)^","^(snd (c_of_atom ct.ctype))^",cfunc)\n"^
  "\t\t#undef cfunc\n"^
  (List.fold_left (^) "" (List.map (fun x -> "\t\t#undef "^x^"\n") idlist))

let c_of_cfunc_calc calclst ct = if (ct.cismat) then "" else
  "\tdo_"^ct.cname^":\n" ^
  (inner_c_of_cfunc_calc calclst ct)^
  "\t\tcontinue;\n"


let c_of_convData cvdata =
  let kernId, chanref, calclst, idx = cvdata in
  let chk_wrapper ct = if ct.cismat then "" else "\tclam_convfunc_chk("^ct.cname^")\n" in
  "\nclam_convfunc_start("^(string_of_int idx)^","^(id_of_imgId (fst chanref))^","^(snd chanref)^")\n"^
  (List.fold_left (^) "" (List.map chk_wrapper calclst))^
  "\t\tclam_convfunc_lastchk()\n"^
  (List.fold_left (^) "" (List.map (c_of_cfunc_calc calclst) calclst))^
  "clam_convfunc_end("^(string_of_int idx)^")"

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

let is_kernel_used id unused = if (List.exists (fun c -> c = id) unused) then "0" else "1"

let rec c_of_kernCalc unused = function
    [] -> "clam_kernel_alloc()"
  | hd :: tl -> "clam_kernel_addcalc("^(c_of_kernCalc unused tl)^","^
  (id_of_calcId hd)^","^(is_kernel_used hd unused)^")"


let c_of_declare_matrix cid t mat =
  let ident = id_of_calcId cid in
  let (bigSz, smallSz) = c_of_atom t in
  ident ^ " = clam_calc_alloc(\"" ^ cid ^ "\", " ^ bigSz ^ ");\n  " ^
  "clam_calc_setmatrix(" ^ ident ^ ", " ^ smallSz ^ ", " ^ (c_of_matrix mat) ^ ")"

let c_of_declare_cstring cid t str ids =
  let ident = id_of_calcId cid in
  ident ^ " = clam_calc_alloc(\"" ^ cid ^ "\", " ^ (fst (c_of_atom t)) ^ ")"

let rec c_of_calcEx = function
    CChain(ca) -> c_of_calcAssign ca
  | CIdent(id,t) -> id_of_calcId id
  | _ -> raise(Failure("Backend found an unnamed Calc expression where it shouldn't be?"))

and c_of_calcAssign ca =
  match ca.c_rhs with
    CMatrix(m) -> c_of_declare_matrix ca.c_lhs ca.c_typ m
  | CRaw(s,ids) -> c_of_declare_cstring ca.c_lhs ca.c_typ s ids
  | _ -> raise(Failure("Backend trying to assign Calc to non-matrix and non-cstring?"))
  
let rec c_of_kernEx = function
    KCalcList(ids,unused) -> c_of_kernCalc unused ids
  | KChain(ka) -> c_of_kernAssign ka
  | KAppend(kap) -> c_of_kernAppend kap
  | KIdent(id) -> id_of_kernId id

and c_of_kernAssign ka =
  let kernel_needs_cloning = function
      KCalcList(_,_) -> false
    | KChain(_) -> true
    | KAppend(_) -> false
    | KIdent(_) -> true
  in
  let c_rhs =
    if (kernel_needs_cloning ka.k_rhs)
    then "clam_kernel_copy( (" ^ (c_of_kernEx ka.k_rhs) ^ ") )"
    else c_of_kernEx ka.k_rhs
  in
  "clam_kernel_assign(" ^ (id_of_kernId ka.k_lhs) ^ ", (" ^ c_rhs ^ ") )"


and c_of_kernAppend kap =
  "TEMP_clam_kernel_addcalc(" ^ (id_of_kernId kap.ka_lhs) ^ ", (" ^ (c_of_calcEx kap.ka_rhs) ^ ") )"

let c_of_conv kid idx =
  "__convolution"^(string_of_int idx)^"("^(id_of_kernId kid)^")"

let c_of_imgread fid =
  "imgread(" ^ (c_of_fid fid) ^ ")"

let name_of_calcEx = function
    CChain(ca) -> ca.c_lhs,ca.c_typ
  | CIdent(cid,t) -> cid,t
  | _ -> raise(Failure("Backend can't find name of calc expression"))

let rec c_of_imAssign ia =
  let img_needs_cloning = function
      ImConv(_,_,_,_) -> false
    | ImRead(_)    -> false
    | ImChain(_)    -> true
    | ImAppend(_)  -> true
    | ImIdent(_)    -> true
  in
  let c_rhs =
    if (img_needs_cloning ia.i_rhs)
    then "clam_img_copy( (" ^ (c_of_imgEx ia.i_rhs) ^ ") )"
    else c_of_imgEx ia.i_rhs
  in
  "clam_img_assign(" ^ (id_of_imgId ia.i_lhs) ^ ", " ^ c_rhs ^ ")"

and c_of_imAppend iap =
  let cid,ctype = name_of_calcEx iap.ia_rhs in
  (* XXX: This is a hack and a shortcut - should be fixed later... *)
  let calcObj = Environ.calct_of_id !Semantic.scope.venv cid in
  let imgt = Environ.imgt_of_id !Semantic.scope.venv iap.ia_lhs in
  let calclst = List.map snd imgt.ichannels in
  "({ {\n"^
  "\t\tclam_img *__IMG = "^(id_of_imgId iap.ia_lhs)^";\n"^
  (inner_c_of_cfunc_calc calclst calcObj) ^
  "\n\t}; "^(id_of_imgId iap.ia_lhs)^"; })"


and c_of_imgEx = function
    ImConv(kid,_,_,idx) -> c_of_conv kid idx
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
    (List.fold_left (^) "" (List.map c_of_calcDecl venv.calc)) ^
    (List.fold_left (^) "" (List.map c_of_convData scope.cvdata))

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

