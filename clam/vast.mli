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


(* Identifiers *)
type calcId = string
type kernId = string
type imgId = string
type filenameId = Const of string | Arg of int
type chanRefId = imgId * calcId

(* Assignment to a Calc *)
type calcAssign = { lhs: calcId; rhs: calcEx; }
and calcEx =
    CMatrix of Ast.matrix
  | CRaw of string * calcId list
  | CChain of calcAssign
  | CIdent of calcId

(* Assignment to a Kernel *)
type kernAppend = { lhs: kernId; rhs: calcEx; }
type kernAssign = { lhs: kernId; rhs: kernEx; }
and kernEx =
    KCalcList of calcId list
  | KChain of kernAssign
  | KIdent of kernId

(* Assignment to an Image *)
type imgAppend = { lhs: imgId; rhs: calcEx; }
type imgAssign = { lhs: imgId; rhs: imgEx; }
and imgEx =
    ImConv of chanRefId * kernEx
  | ImRead of filenameId
  | ImChain of imgAssign
  | ImIdent of imgId

(* Assignment to a Channel Reference *)
type chanAssign = { lhs: chanRefId; rhs: chanRefEx; }
and chanRefEx =
    ChanChain of chanAssign
  | ChanIdent of chanRefId

(* Output images *)
type imgType = Png
type imgWrite = { im: imgEx; fil: filenameId; typ: imgType; }

