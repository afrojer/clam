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

(*
 * XXX - we will need to build-up the environment/context here...
 *       (actually, it would be nice to build it up once here and
 *        then pass it back out to be used in generate_c function)
 *)
let rec check_expr = function
    Id(i) -> Id(i)
  | Integer(BInt(i)) -> Integer(BInt(i))
  | LitStr(s) -> LitStr(s)
  | CStr(s) -> CStr(s)
  | KernCalc(k) -> KernCalc(k)
  | ChanEval(c) -> ChanEval(c)
  | ChanMat(scale,rows) -> ChanMat(scale,rows)
  | ChanRef(c) -> ChanRef(c)
  | Convolve(a,b) -> Convolve(a,b)
  | Assign(i,op,v) -> Assign(i,op,v)
  | ChanAssign(ref,v) -> ChanAssign(ref,v)
  | LibCall(f,args) -> LibCall(f,args)


let check_stmt = function
    Expr(e) -> let vexpr = check_expr e in Expr(vexpr)
  | VDecl(v) -> VDecl(v)
  | VAssign(v,op,e) -> VAssign(v,op,e)
(*
  | VDecl(v) -> let vdecl, env = check_vdecl env v in vdecl
  | VAssign(v,op,e) -> let vdecl, env = check_vassign env VAssign(v,op,e) in
*)


let verify program = List.map check_stmt program

