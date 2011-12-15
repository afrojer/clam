(*
 * File: ast.mli
 * Author: Jeremy C. Andrus <jeremya@cs.columbia.edu>
 * Date: 2011-10-10
 *
 * PLT Fall 2011
 * Homework 1
 * Problem 3
 *)

type atom = Uint8 | Uint16 | Uint32 | Int8 | Int16 | Int32 | Angle
type assign_op = Eq | OrEq | DefEq

type libfunc = ImgRead | ImgWrite

type calcfunc = { func : string; channels : string list; }

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
  | Convolve of expr * expr
  | Assign of string * assign_op * expr
  | ChanAssign of chanref * expr
  | LibCall of libfunc * expr list

type vdecl =
    ImageT of string
  | KernelT of string
  | KCalcT of kerncalc (* need this to keep used/unused list around! *)
  | ConvT of expr * expr
  | CalcT of string * atom
  | StrT of string * string
  | BareT of string (* used in typechecking: see type_of_expr *)

type stmt =
    Expr of expr
  | VDecl of vdecl
  | VAssign of vdecl * assign_op * expr

type program = stmt list
