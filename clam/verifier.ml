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
open Clamtypes

let image_has channel imgnm env =
  let eImg = (List.find
               (fun i -> if i.iname = imgnm then true else false)
               env.images)
  in
    if (List.exists
         (fun (nm,calc) -> if nm = channel then true else false)
         eImg.ichannels)
    then ()
    else (raise (Failure("No channel '"^channel^"' in image '"^imgnm^"'")))


let check_chanref env cref =
  if not (type_of cref.image env = ImageT(cref.image)) then
    (raise (Failure("Channel reference ("^cref.channel^
                    ") on non-image: "^cref.image)))
  else
    image_has cref.channel cref.image env


let rec check_expr env = function
    Id(i) -> let _ = type_of i env in env, Id(i)
  | Integer(BInt(i)) -> env, Integer(BInt(i))
  | LitStr(s) -> env, LitStr(s)
  | CStr(s) -> env, CStr(s)
  | KernCalc(k) -> env, KernCalc(k)
  | ChanEval(c) -> env, ChanEval(c)
  | ChanMat(m) -> env, ChanMat(m)
  | ChanRef(c) -> check_chanref env c; env, ChanRef(c)
  | Convolve(a,b) -> env, Convolve(a,b)
  (* XXX: create a function 'check_assign' to use here
   *      (and in VAssign?)
   *)
  | Assign(i,op,v) -> let _ = type_of i env in
                      let env1, ve = check_expr env v in
                      env1, Assign(i,op,ve)
  | ChanAssign(ref,v) -> (* check_chanref env ref;
                            need to dynamically add the channel! *)
                        let env1, ve = check_expr env v in
                        env1, ChanAssign(ref,ve)
  | LibCall(f,args) -> env, LibCall(f,args)


let check_stmt env = function
    Expr(e) -> let env1, vexpr = check_expr env e in
                env1, Expr(vexpr)
  | VDecl(v) -> let env1 = var_add env v in
                env1, VDecl(v)
  (* XXX: verify that the result of the expression
   *      matches the type of the variable
   *)
  | VAssign(v,op,e) -> let env1 = var_add env v in
                let env2, vexpr = check_expr env1 e in
                env2, VAssign(v,op,vexpr)


let verify program =
  let venv, vslist = (List.fold_left
    (fun (envO,slist) s -> let env, vstmt =
                             check_stmt envO s in env, vstmt :: slist)
    ( {calc = [];
       images = [];
       kernels = []}, [] ) (List.rev program))
  in
  venv, (List.rev vslist)
