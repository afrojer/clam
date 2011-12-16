(*
 * File: verifier.ml
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

open Ast
open Sast
open Envtypes
open Environ
open Printer

(* Strategy:
 * trans_N ENV N =
 *   1) Make sure N has the correct child nodes
 *   2) Check the child nodes
 *   3) Update the ENV for any effects from N
 *   4) Create VAST, the vast node that represents N
 *   5) Return (ENV, VAST)
 *)

exception SemanticFailure of string

let scope = ref { venv = { calc = []; images = []; kernels = [] }; }

let type_of_vdecl = function
    ImageT(s) -> ImageType
  | KernelT(s) -> KernelType
  | KCalcT(kc) -> raise(SemanticFailure("Kernel Calc does not have an associated type. Does this VDecl even exist!?"))
  | ConvT(e1,e2)-> ImageType
  | CalcT(s,t) -> CalcType(t)
  | _ -> raise(SemanticFailure("Invalid use of internal type."))

let type_of_vexpr = function
    CalcEx(e) -> CalcType(Unknown) (* XXX: We have problems in how we handle the atomic types of Calcs *)
  | KernelEx(e) -> KernelType
  | ImageEx(e) -> ImageType
  | ChanRefEx(e) -> ChanRefType
  | ImgWriteEx(im,f,fi) -> VoidType
  | Debug(s) -> print_endline("XXX: pretending Debug is a type CalcType(Unknown)"); CalcType(Unknown)


let ident_of_vdecl = function
    ImageT(s) -> s
  | CalcT(s,t) -> s
  | KernelT(s) -> s
  | KCalcT(kc) -> raise(SemanticFailure("Kernel Calc does not have an associated identifier string"))
  | ConvT(e1,e2)-> raise(SemanticFailure("Convolution does not have an associated identifier string"))
  | _ -> raise(SemanticFailure("Invalid use of internal type!"))

let type_of_ident scope s = 
  type_of_vdecl (Environ.type_of !scope.venv s)

let int_of_BInt = function
  BInt(i) -> i


(*
 * Recursive Checking Functions
 *)

(* Returns: filenameId *)
let filenameId_of_expr = function
    Integer(bi) -> let i = int_of_BInt(bi) in Arg(i)
  | LitStr(s) -> Const(s)
  | _ -> raise(SemanticFailure("Filenames can only be a string or an integer"))

(* Returns fmtType *)
let fmtType_of_expr = function
    LitStr(s) -> (match s with
        "png" -> Png
      | _ -> raise(SemanticFailure("Unknown image format: " ^ s))
    )
  | _ -> raise(SemanticFailure("Image format must be specified as a string literal"))

(* Returns: vExpr *)
let trans_id s =
  let typ = type_of_ident scope s in
    match typ with
        CalcType(t) -> CalcEx(CIdent({ cid = s; }))
      | KernelType  -> KernelEx(KIdent({ kid = s; }))
      | ImageType   -> ImageEx(ImIdent({ iid = s; }))
      | _ -> raise(SemanticFailure("Environment claimed identifier was non-standard type"))

(* Returns: CMatrix *)
let cMatrix_of_matrix m =
  let ((bNum, bDen), bColRow) = m in
    let num = int_of_BInt bNum in
    let den = int_of_BInt bDen in
    let colRow = List.map (List.map int_of_BInt) bColRow in
      CMatrix((num, den), colRow)



(* Returns: chanRefId *)
let trans_chanRefIdLval ch = ( { iid = ch.image }, { cid = ch.channel } )

(* Returns: chanRefId *)
let trans_chanRefId ch = ( { iid = ch.image }, { cid = ch.channel } )

(* Returns: vExpr *)
let trans_imgread elist = ImageEx(ImRead(filenameId_of_expr (List.hd elist)))


(* Returns: vExpr *)
let rec trans_libf libf elist =
  match libf with
     ImgRead -> trans_imgread elist
   | ImgWrite -> trans_imgwrite elist

(* Returns: vExpr *)
and trans_imgwrite elist =
  match elist with
      img_expr :: raw_format :: raw_filename :: [] -> (
        let imgEx =
          let vexpr = trans_expr img_expr in
            match vexpr with
                ImageEx(imgExpr) -> imgExpr
              | _ -> raise(SemanticFailure("1st argument to ImgWrite must be an Image expression"))
        in
        let fmt = fmtType_of_expr raw_format in
          let file = filenameId_of_expr raw_filename in
            ImgWriteEx(imgEx, fmt, file)
      )
    | _ -> raise(SemanticFailure("Wrong number of arguments supplied to imgwrite function"))

(* Returns: ImConv *)
and trans_conv e1 e2 =
  let ve1 = trans_expr e1 in
    let ve2 = trans_expr e2 in
      let chrefId = (match ve1 with
          ChanRefEx(cv) -> (match cv with
                                ChanIdent(cid) -> cid
                              | ChanChain(ca) -> raise(SemanticFailure("Must supply a channel identifier to a convolve operation"))
                           )
        | _ -> raise(SemanticFailure("Convolutions must have a ChanRef on the left-hand side"))
        )
      in
      let kernEx = match ve2 with
          KernelEx(ke) -> (match ke with
                               KCalcList(cids) -> raise(SemanticFailure("Cannot convolve with an unnamed Kernel"))
                             | KChain(ka) -> ke
                             | KAppend(ka) -> ke
                             | KIdent(kid) -> ke
                          )
        | _ -> raise(SemanticFailure("Convolutions must have a kernel on the right-hand side"))
      in
      ImConv(chrefId, kernEx)

(* Returns: vExpr *)
and trans_expr = function
    Id(s) -> trans_id s
  | CStr(s,ids) -> CalcEx(CRaw(s, (List.map (fun s -> { cid = s; }) ids)))
  | KernCalc(kc) -> KernelEx(KCalcList((List.map (fun x -> {cid = x}) kc.allcalc)))
  | ChanMat(m) -> CalcEx(cMatrix_of_matrix m)
  | ChanRef(ch) -> ChanRefEx(ChanIdent(trans_chanRefId ch))
  | Convolve(e1,e2) -> ImageEx(trans_conv e1 e2)
  | Assign(s,op,e) -> trans_assign s op e
  | ChanAssign(ch, e) -> (let chId = trans_chanRefIdLval ch in
                            let ve = trans_expr e in
                              match ve with
                                  ChanRefEx(cve) -> ChanRefEx(ChanChain({ch_lhs = chId; ch_rhs = cve;}))
                                | _ -> raise(SemanticFailure("Must assign Channel to a channel type"))
                         )
  | LibCall(libf, elist) -> trans_libf libf elist
  | _ -> raise(SemanticFailure("Encountered AST Expression node that we didnt know how to verify"))

(* Returns: vExpr *)
and trans_eq_assign s e =
  let ve = trans_expr e in
    match (type_of_ident scope s) with
        CalcType(t) -> (match ve with CalcEx(ce) -> CalcEx(CChain({ c_lhs = {cid = s}; c_rhs = ce; })) | _ -> raise(SemanticFailure("Bad assignment")))
      | KernelType -> (match ve with KernelEx(ke) -> KernelEx(KChain({ k_lhs = {kid = s}; k_rhs = ke; })) | _ -> raise(SemanticFailure("Bad assignment")))
      | ImageType -> (match ve with ImageEx(ie) -> ImageEx(ImChain({ i_lhs = {iid = s}; i_rhs = ie; })) | _ -> raise(SemanticFailure("Bad assignment")))
      | _ -> raise(SemanticFailure("Identifier claims to be an impossible data type"))

and trans_or_assign s e =
  let ve = trans_expr e in
    match ve with
        CalcEx(c) -> (match (type_of_ident scope s) with
                  KernelType -> KernelEx(KAppend({ ka_lhs = { kid = s }; ka_rhs = c; }))
                | ImageType -> ImageEx(ImAppend({ ia_lhs = { iid = s }; ia_rhs = c; }))
                | _ -> raise(SemanticFailure("OrEq operation must have Kernel or Image as its L-Value"))
              )
      | _ -> raise(SemanticFailure("Unexpected expression is an R-Value for OrEq operation"))


(* Returns: vExpr *)
and trans_def_assign s e = match (trans_expr e) with
    CalcEx(cexp) -> CalcEx(CChain({ c_lhs = { cid=s }; c_rhs = cexp; }))
  | _ -> raise(SemanticFailure("DefEq to something not a Calc expression"))

(* Returns: vExpr *)
and trans_assign s op e =
  match op with
      Eq -> trans_eq_assign s e
    | OrEq -> trans_or_assign s e
    | DefEq -> trans_def_assign s e

let trans_vdecl = function
    ImageT(s)  -> Debug("Declare Image")
  | KernelT(s) -> Debug("Declare Kernel")
  | CalcT(s,t) -> Debug("Declare Calc")
  | _ -> raise(SemanticFailure("A variable declaration did not have a recognizable type"))

(* Returns: vExpr *)
let trans_action_expr expr = match expr with
    Assign(s,op,e) -> trans_expr expr
  | ChanAssign(chref,e) -> trans_expr expr
  | LibCall(libf,elist) -> trans_expr expr
  | _ -> Debug("Expression result ignored!") (* TODO: Can side effects be hiding in the expression? *)

let trans_stmt = function
    Expr(e) -> trans_action_expr e
  | VDecl(v) -> trans_vdecl v
  | VAssign(v,op,e) -> (
      let _ = trans_vdecl v in
        trans_assign (ident_of_vdecl v) op e
    )

let translate_ast env ast =
  scope.contents <- { venv = env; };
  let gather nodes stmt = (trans_stmt stmt) :: nodes in
    let nodelist = List.fold_left gather [] ast in
      (!scope, List.rev nodelist)

