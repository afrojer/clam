(*
 * File: vast.ml
 * Date: 2011-10-16
 *
 * PLT Fall 2011
 * CLAM Project
 * Jeremy C. Andrus <jeremya@cs.columbia.edu>
 * Robert Martin <rdm2128@columbia.edu>
 * Kevin Sun <kfs2110@columbia.edu> * Yongxu Zhang <yz2419@columbia.edu>
 *
 *)



(* Environment Objects *)
type fmtType = Png
type typeT = CalcType of Ast.atom
           | KernelType
           | ImageType
           | ChanRefType
           | FilenameType
           | FormatType
           | VoidType
type identT = {
  id: string;
  typ: typeT;
  init: bool;
  chans: string list; (* Only relevant for image identifiers *)
}

type envT = {
  ids: identT list;
}


(* Identifiers *)
type calcId = { cid: string; }
type kernId = { kid: string; }
type imgId = { iid: string; }
type chanRefId = imgId * calcId
type filenameId = Const of string | Arg of int

(* Assignment to a Calc *)
type calcAssign = { c_lhs: calcId; c_rhs: calcEx; }
and calcEx =
    CMatrix of Ast.matrix
  | CRaw of string * calcId list
  | CChain of calcAssign
  | CIdent of calcId

(* Assignment to a Kernel *)
type kernAppend = { ka_lhs: kernId; ka_rhs: calcEx; }
type kernAssign = { k_lhs: kernId; k_rhs: kernEx; }
and kernEx =
    KCalcList of calcId list
  | KChain of kernAssign
  | KIdent of kernId

(* Assignment to an Image *)
type imgAppend = { ia_lhs: imgId; ia_rhs: calcEx; }
type imgAssign = { i_lhs: imgId; i_rhs: imgEx; }
and imgEx =
    ImConv of chanRefId * kernEx
  | ImRead of filenameId
  | ImChain of imgAssign
  | ImIdent of imgId

(* Assignment to a Channel Reference *)
type chanAssign = { ch_lhs: chanRefId; ch_rhs: chanRefEx; }
and chanRefEx =
    ChanChain of chanAssign
  | ChanIdent of chanRefId

(* Output images *)
type imgWrite = { im: imgEx; fil: filenameId; fmtType: fmtType; }


type vExpr =
    CalcEx of calcEx
  | KernelEx of kernEx
  | ImageEx of imgEx
  | ChanRefEx of chanRefEx
  | FilenameEx of filenameId
  | FormatEx of fmtType
  | ImgWriteEx of imgEx * fmtType * filenameId
  | Debug of string
 

type vastRoot = vExpr list

