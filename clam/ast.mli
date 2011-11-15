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

type libfunc = ImgRead | ImgWrite

type chanref = { image : string; channel : string; }

type kerncalc = string list * string list

type vdecl =
    ImageT of string
  | KernelT of string
  | CalcT of string * atom

type expr =
    Id of string
  | Integer of int
  | LitStr of string
  | CStr of string
  | KernCalc of kerncalc
  | ChanEval of chanref
  | ChanRef of chanref
  | Convolve of expr * expr
  | Assign of string * expr
  | ChanAssign of chanref * expr
  | LibCall of libfunc * expr list

type stmt =
    Expr of expr
  | VDecl of vdecl
  | VDef of vdecl * expr

type program = stmt list
