(*
 * File: ast.mli
 * Date: 2011-10-10
 *
 * PLT Fall 2011
 * CLAM Project
 * Jeremy C. Andrus <jeremya@cs.columbia.edu>
 * Robert Martin <rdm2128@columbia.edu>
 * Kevin Sun <kfs2110@columbia.edu>
 * Yongxu Zhang <yz2419@columbia.edu>
 *)

type atom = Uint8 | Uint16 | Uint32 | Int8 | Int16 | Int32 | Angle | Unknown
type assign_op = Eq | OrEq | DefEq

type libfunc = ImgRead | ImgWrite

type chanref = { image : string; channel : string; }

type kerncalc = { allcalc: string list; unusedcalc: string list }

type bareint =
    BInt of int

type matrix = (bareint * bareint) * bareint list list

type expr =
    Id of string
  | Integer of bareint
  | LitStr of string
  | CStr of string * string list
  | KernCalc of kerncalc
  | ChanMat of matrix
  | ChanRef of chanref
  | Convolve of chanref * string
  | Assign of string * assign_op * expr
  | ChanAssign of chanref * expr
  | LibCall of libfunc * expr list

type vdecl =
    ImageT of string
  | KernelT of string
  | KCalcT of kerncalc (* need this to keep used/unused list around! *)
  | ConvT of chanref * string
  | CalcT of string * atom
  | StrT of string * string
  | BareT of string

type stmt =
    Expr of expr
  | VDecl of vdecl
  | VAssign of vdecl * assign_op * expr

type program = stmt list
