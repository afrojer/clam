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
open Vast
open Environ
open Printer

(* Strategy:
 * check_N ENV N =
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

let string_of_vdecl = function
    ImageT(s) -> s
  | KernelT(s) -> s
  | KCalcT(kc) -> raise(Failure("Kernel Calc does not have an associated identifier string"))
  | ConvT(e1,e2)-> raise(Failure("Convolution does not have an associated identifier string")) 
  | CalcT(s,t) -> s



let check_assign str e =
  Debug("Assign to " ^ str)

let check_orassign str e =
  Debug("OrAssign to " ^ str)

let check_defassign str e =
  Debug("DefAssign to " ^ str)

let check_vdecl = function
    ImageT(s) -> (declare_var scope s ImageType; Debug("Declare Image"))
  | KernelT(s) -> Debug("Declare Kernel")
  | KCalcT(kc) -> Debug("Declare KernelCalc")
  | ConvT(e1,e2) -> Debug("Declare Convolution")
  | CalcT(s,t) -> Debug("Declare Calc")

let check_action_expr = function
    Assign(s,op,e) -> Debug("Assign")
  | ChanAssign(chref,e) -> Debug("ChanAssign")
  | LibCall(libf,elist) -> Debug("LibCall")
  | _ -> raise(Failure("Expression result is ignored"))
    
let check_stmt = function
    Expr(e) -> check_action_expr e
  | VDecl(v) -> check_vdecl v
  | VAssign(v,op,e) -> (
      let _ = check_vdecl v in
        match op with
              Eq    -> check_assign (string_of_vdecl v) e
            | OrEq  -> check_orassign (string_of_vdecl v) e
            | DefEq -> check_defassign (string_of_vdecl v) e
    )

let verify ast =
  let gather nodes stmt = (check_stmt stmt) :: nodes in
    let nodelist = List.fold_left gather [] ast in
      (!scope, nodelist)

