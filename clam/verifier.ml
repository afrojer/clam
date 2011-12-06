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
open Environ

let find_image env imgnm = List.find
  (fun i -> if i.iname = imgnm then true else false)
  env.images

let image_has imgnm channel env simple =
  let eImg = (List.find
               (fun i -> if i.iname = imgnm then true else false)
               env.images)
  in
    if (List.exists
         (fun (nm,calc) -> if nm = channel then true else false)
         eImg.ichannels)
    then ()
    else
      if simple then
        (raise (Failure("NOCHAN")))
      else
        (raise (Failure("No channel '"^channel^"' in image '"^imgnm^"'")))

let image_add img channel typ isvalid ismat =
  (* XXX: add a new calc to the environment?! with cname=img^"."^channel;? *)
  img.ichannels <- (channel,
                     { cname = channel;
                       ctype = typ;
                       cisvalid = isvalid;
                       cismat = ismat;
                       cfunc = "";
                       cmatrix = (BInt(1),BInt(1)),[[BInt(1)]];
                     }) :: img.ichannels;
  ()

let check_chanref env cref simple =
  if not (type_of cref.image env = ImageT(cref.image)) then
    if simple then
      (raise (Failure("NONIMG")))
    else
      (raise (Failure("Channel reference ("^cref.channel^
                      ") on non-image: "^cref.image)))
  else
    image_has cref.image cref.channel env simple


let rec check_expr env = function
    Id(i) -> let _ = type_of i env in env, Id(i)
  | Integer(BInt(i)) -> env, Integer(BInt(i))
  | LitStr(s) -> env, LitStr(s)
  | CStr(s) -> env, CStr(s)
  | KernCalc(k) -> env, KernCalc(k)
  | ChanEval(c) -> check_chanref env c false; env, ChanEval(c)
  | ChanMat(m) -> env, ChanMat(m)
  | ChanRef(c) -> check_chanref env c false; env, ChanRef(c)
  | Convolve(a,b) -> env, Convolve(a,b)
  (* XXX: create a function 'check_assign' to use here
   *      (and in VAssign?)
   *)
  | Assign(i,op,v) -> let _ = type_of i env in
                      let env1, ve = check_expr env v in
                      env1, Assign(i,op,ve)
  | ChanAssign(ref,v) ->
       (try
          check_chanref env ref true
        with
          Failure(s) ->
            if s = "NOCHAN" then
              (* Add channel to image! *)
              image_add (find_image env ref.image) ref.channel Uint8 false false
            else
                (raise (Failure(s)));
       );
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
