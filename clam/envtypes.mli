(*
 * File: env.mli
 * Date: 2011-11-09
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

(* Environment Types *)
type calcT = {
  cname    : string;
  ctype    : Ast.atom;
  mutable cisvalid : bool;
  mutable cismat   : bool; (* if true, use 'matrix' else use 'cfunc' *)
  mutable cfunc    : string * string list;
  mutable cmatrix  : Ast.matrix;
}

type imgT = {
  iname     : string;
  mutable ichannels : (string * calcT) list;
}

type kernelT = {
  kname       : string;
  mutable kallcalc    : string list;
  mutable kunusedcalc : string list;
}

type envT = {
  mutable calc : calcT list;
  images  : imgT list;
  kernels : kernelT list;
}
