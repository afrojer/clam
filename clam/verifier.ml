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

let type_of_vexpr = function
    CalcEx(e) -> CalcType(Unknown) (* XXX: We have big problems in how we handle the atomic types of Calcs *)
  | KernelEx(e) -> KernelType
  | ImageEx(e) -> ImageType
  | ChanRefEx(e) -> ChanRefType
  | FilenameEx(f) -> FilenameType
  | FormatEx(f) -> FormatType


let string_of_vdecl = function
    ImageT(s) -> s
  | CalcT(s,t) -> s
  | KernelT(s) -> s
  | KCalcT(kc) -> raise(Failure("Kernel Calc does not have an associated identifier string"))
  | ConvT(e1,e2)-> raise(Failure("Convolution does not have an associated identifier string")) 


(*
 * Recursive Checking Functions
 *)

let check_expr = function
  _ -> CalcEx(CRaw("[RawString]", []))

let check_eq_assign s e =
  let vexpr = check_expr e in
    env_assign scope s (type_of_vexpr vexpr);
    Debug("Assign to " ^ s)

let check_or_assign s e =
  Debug("OrAssign to " ^ s)

let check_def_assign s e =
  Debug("DefAssign to " ^ s)

let check_assign s op e =
  match op with
      Eq -> check_eq_assign s e
    | OrEq -> check_or_assign s e
    | DefEq -> check_def_assign s e

let check_vdecl = function
    ImageT(s)  -> print_env !scope; env_declare scope s ImageType; Debug("Declare Image")
  | KernelT(s) -> env_declare scope s KernelType; Debug("Declare Kernel")
  | CalcT(s,t) -> env_declare scope s (CalcType(t)); Debug("Declare Calc")
  | _ -> raise(Failure("A variable declaration did not have a recognizable type"))

let check_action_expr = function
    Assign(s,op,e) -> check_assign s op e
  | ChanAssign(chref,e) -> Debug("ChanAssign")
  | LibCall(libf,elist) -> Debug("LibCall")
  | _ -> raise(Failure("Expression result is ignored"))
    
let check_stmt = function
    Expr(e) -> check_action_expr e
  | VDecl(v) -> check_vdecl v
  | VAssign(v,op,e) -> (
      let _ = check_vdecl v in
        check_assign (string_of_vdecl v) op e
    )

let verify ast =
  let gather nodes stmt = (check_stmt stmt) :: nodes in
    let nodelist = List.fold_left gather [] ast in
      (!scope, List.rev nodelist)

