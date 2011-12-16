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
open Env
open Printer

(* Strategy:
 * trans_N ENV N =
 *   1) Make sure N has the correct child nodes
 *   2) Check the child nodes
 *   3) Update the ENV for any effects from N
 *   4) Create VAST, the vast node that represents N
 *   5) Return (ENV, VAST)
 *)

let scope = ref { ids = []; }

let type_of_vdecl = function
    ImageT(s) -> ImageType
  | KernelT(s) -> KernelType
  | KCalcT(kc) -> raise(Failure("Kernel Calc does not have an associated type. Does this VDecl even exist!?"))
  | ConvT(e1,e2)-> ImageType
  | CalcT(s,t) -> CalcType(t)
  | _ -> raise(Failure("Invalid use of internal type."))

let type_of_vexpr = function
    CalcEx(e) -> CalcType(Unknown) (* XXX: We have problems in how we handle the atomic types of Calcs *)
  | KernelEx(e) -> KernelType
  | ImageEx(e) -> ImageType
  | ChanRefEx(e) -> ChanRefType
  | FilenameEx(f) -> FilenameType
  | FormatEx(f) -> FormatType
  | ImgWriteEx(im,f,fi) -> VoidType
  | Debug(s) -> print_endline("XXX: pretending Debug is a type CalcType(Unknown)"); CalcType(Unknown)


let ident_of_vdecl = function
    ImageT(s) -> s
  | CalcT(s,t) -> s
  | KernelT(s) -> s
  | KCalcT(kc) -> raise(Failure("Kernel Calc does not have an associated identifier string"))
  | ConvT(e1,e2)-> raise(Failure("Convolution does not have an associated identifier string"))
  | _ -> raise(Failure("Invalid use of internal type!"))

let int_of_BInt = function
  BInt(i) -> i


(*
 * Recursive Checking Functions
 *)

(* Returns: filenameId *)
let trans_filenameId_expr = function
    Integer(bi) -> let ix = match bi with BInt(i) -> i in Arg(ix)
  | LitStr(s) -> Const(s)
  | _ -> raise(Failure("Filenames can only be a string or an integer"))

(* Returns fmtType *)
let trans_format_expr = function
    LitStr(s) -> (match s with
        "png" -> Png
      | _ -> raise(Failure("Unknown image format: " ^ s))
    )
  | _ -> raise(Failure("Image format must be specified as a string literal"))

(* Returns: vExpr *)
let trans_id s =
  let typ = env_type_of_ident scope s in
    match typ with
        CalcType(t) -> CalcEx(CIdent({ cid = s; }))
      | KernelType  -> KernelEx(KIdent({ kid = s; }))
      | ImageType   -> ImageEx(ImIdent({ iid = s; }))
      | _ -> raise(Failure("Environment claimed identifier was non-standard type"))

(* Returns: CMatrix *)
let trans_mat m =
  let ((bNum, bDen), bColRow) = m in
    let num = int_of_BInt bNum in
    let den = int_of_BInt bDen in
    let colRow = List.map (List.map int_of_BInt) bColRow in
    let _ = if (den = 0) then raise(Failure("Division by zero in matrix denominator"))
      else match (List.map List.length colRow) with
          [] -> raise(Failure("Cannot have an empty matrix"))
        | hd :: tl -> List.fold_left (fun x y -> if (x = y && x > 0) then x else raise(Failure("Uneven matrix row lengths"))) hd tl
    in
    CMatrix((num, den), colRow)



(* Returns: chanRefId *)
let trans_chanRefIdLval ch =
  env_assign_chan scope ch;
  ( { iid = ch.image }, { cid = ch.channel } )

(* Returns: chanRefId *)
let trans_chanRefId ch =
  env_exists_chan scope ch;
  ( { iid = ch.image }, { cid = ch.channel } )


(* Returns: vExpr *)
let trans_imgread elist =
  match elist with
      raw_arg :: [] -> (
        let fileId = trans_filenameId_expr raw_arg in
          ImageEx(ImRead(fileId))
      )
    | _ -> raise(Failure("Wrong number of arguments supplied to imgread function"))




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
              | _ -> raise(Failure("1st argument to ImgWrite must be an Image expression"))
        in
        let fmt = trans_format_expr raw_format in
          let file = trans_filenameId_expr raw_filename in
            ImgWriteEx(imgEx, fmt, file)
      )
    | _ -> raise(Failure("Wrong number of arguments supplied to imgwrite function"))

(* Returns: ImConv *)
and trans_conv e1 e2 =
  let ve1 = trans_expr e1 in
    let ve2 = trans_expr e2 in
      let chrefId = (match ve1 with
          ChanRefEx(cv) -> (match cv with
                                ChanIdent(cid) -> cid
                              | ChanChain(ca) -> raise(Failure("Must supply a channel identifier to a convolve operation"))
                           )
        | _ -> raise(Failure("Convolutions must have a ChanRef on the left-hand side"))
        )
      in
      let kernEx = match ve2 with
          KernelEx(ke) -> (match ke with
                               KCalcList(cids) -> raise(Failure("Cannot convolve with an unnamed Kernel"))
                             | KChain(ka) -> ke
                             | KAppend(ka) -> ke
                             | KIdent(kid) -> ke
                          )
        | _ -> raise(Failure("Convolutions must have a kernel on the right-hand side"))
      in
      ImConv(chrefId, kernEx)

(* Returns: vExpr *)
and trans_expr = function
    Id(s) -> trans_id s
  | CStr(s,ids) -> CalcEx(CRaw(s, (List.map (fun s -> { cid = s; }) ids)))
  | KernCalc(kc) -> KernelEx(KCalcList((List.map (fun x -> {cid = x}) kc.allcalc)))
  | ChanMat(m) -> CalcEx(trans_mat m)
  | ChanRef(ch) -> ChanRefEx(ChanIdent(trans_chanRefId ch))
  | Convolve(e1,e2) -> ImageEx(trans_conv e1 e2)
  | Assign(s,op,e) -> (match (env_type_of_ident scope s) with
                           CalcType(t) -> (env_assign scope s (CalcType t);
                                           let ve = trans_expr e in match ve with
                                               CalcEx(calcEx) -> CalcEx(CChain({ c_lhs = {cid = s}; c_rhs = calcEx; }))
                                             | _ -> raise(Failure("Must assign Calc type to calc identifier"))
                                          )
                         | KernelType  -> (env_assign scope s KernelType;
                                           let ve = trans_expr e in match ve with
                                               KernelEx(kernEx) -> KernelEx(KChain({ k_lhs = {kid = s}; k_rhs = kernEx; }))
                                             | _ -> raise(Failure("Must assign Kernel type to kernel identifier"))
                                          )
                         | ImageType   -> (env_assign scope s ImageType;
                                           let ve = trans_expr e in match ve with
                                               ImageEx(imgEx) -> ImageEx(ImChain({ i_lhs = {iid = s}; i_rhs = imgEx; }))
                                             | _ -> raise(Failure("Must assign Image type to image identifier"))
                                          )
                         | _ -> raise(Failure("Identifier claims to be an impossible data type"))
                      )
  | ChanAssign(ch, e) -> (let chId = trans_chanRefIdLval ch in
                            let ve = trans_expr e in
                              match ve with
                                  ChanRefEx(cve) -> ChanRefEx(ChanChain({ch_lhs = chId; ch_rhs = cve;}))
                                | _ -> raise(Failure("Must assign Channel to a channel type"))
                         )
  | LibCall(libf, elist) -> trans_libf libf elist
  | _ -> raise(Failure("Encountered AST Expression node that we didnt know how to verify"))

let trans_eq_assign s e =
  let ve = trans_expr e in
    env_assign scope s (type_of_vexpr ve);
    (* TODO: Create Sast for '=' operator *)
    Debug("Assign to " ^ s)

let trans_or_assign s e =
  let ve = trans_expr e in
    match ve with
        CalcEx(c) -> (match (env_type_of_ident scope s) with
                  KernelType -> KernelEx(KAppend({ ka_lhs = { kid = s }; ka_rhs = c; }))
                | ImageType -> ImageEx(ImAppend({ ia_lhs = { iid = s }; ia_rhs = c; }))
                | _ -> raise(Failure("OrEq operation must have Kernel or Image as its L-Value"))
              )
      | _ -> raise(Failure("Unexpected expression is an R-Value for OrEq operation"))

let trans_def_assign s e =
  let ve = trans_expr e in
    match ve with
        CalcEx(c) -> env_assign scope s (type_of_vexpr ve); CalcEx(c)
      | _ -> raise(Failure("Can only DefAssign a Calc"))

let trans_assign s op e =
  match op with
      Eq -> trans_eq_assign s e
    | OrEq -> trans_or_assign s e
    | DefEq -> trans_def_assign s e

let trans_vdecl = function
    ImageT(s)  -> env_declare scope s ImageType; Debug("Declare Image")
  | KernelT(s) -> env_declare scope s KernelType; Debug("Declare Kernel")
  | CalcT(s,t) -> env_declare scope s (CalcType(t)); Debug("Declare Calc")
  | _ -> raise(Failure("A variable declaration did not have a recognizable type"))

let trans_action_expr = function
    Assign(s,op,e) -> trans_assign s op e
  | ChanAssign(chref,e) -> Debug("ChanAssign")
  | LibCall(libf,elist) -> Debug("LibCall")
  | _ -> Debug("Expression result ignored!")

let trans_stmt = function
    Expr(e) -> trans_action_expr e
  | VDecl(v) -> trans_vdecl v
  | VAssign(v,op,e) -> (
      let _ = trans_vdecl v in
        trans_assign (ident_of_vdecl v) op e
    )

let translate_ast ast =
  let gather nodes stmt = (trans_stmt stmt) :: nodes in
    let nodelist = List.fold_left gather [] ast in
      (!scope, List.rev nodelist)

