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
open Printer

(*
 * Find an image by name from the environment
 *)
let find_image env imgnm = List.find
  (fun i -> if i.iname = imgnm then true else false)
  env.images

(*
 * Check if an image has a channel with the specified name
 *)
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

let get_calctype env chname =
  let c = (List.find (fun i -> if i.cname = chname then true else false)
                     env.calc)
  in c.ctype


(*
 * Add a channel to the specified image
 * (the caller needs to initialize most of the member variables)
 *)
let image_add env img channel typ isvalid ismat cfuncstr cmat =
  img.ichannels <- (channel,
                     { cname = channel;
                       ctype = typ;
                       cisvalid = isvalid;
                       cismat = ismat;
                       cfunc = cfuncstr;
                       cmatrix = cmat;
                     }) :: img.ichannels;
  (* Add a new CalcT to the environment named "image:channel" *)
  var_add env (CalcT(img.iname^":"^channel, typ))

(*
 * VERIFY:
 *   our basic type checker!
 *   checks for a valid assignment from rhs to lhs
 *   i.e. it checks the expression:
 *              lhs = rhs
 *        for validity
 *)
let check_assignment env rhs op = function (* passes in LHS *)
    ImageT(nm) ->
        let optype rhs = function
            DefEq -> (raise (Failure("Cannot define "^
                                     (string_of_type (ImageT(nm)))^" with ':='")))
          | Eq -> if not (rhs = ImageT(":i")) then
                    (raise (Failure("Can't assign '"^(string_of_type rhs)^
                                    "' to "^(string_of_type (ImageT(nm)))^":"^
                                    "you can only assign Images to Images")))
                  else env
          | OrEq ->
                let chk_chan_add = function
                    CalcT(nm,t) -> nm, t
                  | _ as t -> (raise (Failure("Can't assign "^
                               (string_of_type t)^" to "^(string_of_type (ImageT(nm)))^
                               ": Invalid image channel for |=")))
                in
                let chname, chtype = chk_chan_add rhs in
                let env1 =
                  image_add env (find_image env nm) chname chtype
                            false false "" ((BInt(1),BInt(1)),[[BInt(1)]])
                in env1
        in optype rhs op
  | KernelT(nm) -> env
  | CalcT(nm,t) -> env
  | StrT(t,s) -> (raise (Failure("Can't assign to a string: '"^s^"'")))
  | BareT(s) -> (raise (Failure("Cannot assign to '"^s^"'")))

(*
 * VERIFY:
 *   a channel reference needs to reference a valid image, and must
 *   already be a defined channel on that image
 *)
let check_chanref env cref simple =
  if not (type_of env cref.image = ImageT(cref.image)) then
    if simple then
      (raise (Failure("NONIMG")))
    else
      (raise (Failure("Channel reference ("^cref.channel^
                      ") on non-image: "^cref.image)))
  else
    image_has cref.image cref.channel env simple

(*
 * VERIFY:
 *   a convolution statement
 *   i.e. it checks validity of "Channel ** Kernel" statements
 *        and retuns a list of new channels produced by the
 *        convolution (so we can add them to the resulting image)
 *)
let check_convolve env chanref kern =
  (* XXX: add kern channels to resulting image! *)
  env

(*
 * VERIFY:
 *   our expression checker
 *)
let rec check_expr env = function
    Id(i) -> let _ = type_of env i in env, Id(i)
  | Integer(BInt(i)) -> env, Integer(BInt(i))
  | LitStr(s) -> env, LitStr(s)
  | CStr(s) -> env, CStr(s)
  | KernCalc(k) -> env, KernCalc(k)
  | ChanEval(c) -> check_chanref env c false; env, ChanEval(c)
  | ChanMat(m) -> env, ChanMat(m)
  | ChanRef(c) -> check_chanref env c false; env, ChanRef(c)
  | Convolve(a,b) ->
        let env1, va = check_expr env a in
        let env2, vb = check_expr env1 b in
        let env3 = check_convolve env2 va vb in
        env3, Convolve(va,vb)
  | Assign(i,op,v) ->
        let lhs = type_of env i in
        let env1, vexpr = check_expr env v in
        let rhs = type_of_expr env1 vexpr in
        let env2 = check_assignment env1 rhs op lhs in
        env2, Assign(i,op,vexpr)
  | ChanAssign(ref,v) ->
        let env1, ve = check_expr env v in
        let envNew =
          (try check_chanref env1 ref true; env
           with Failure(s) ->
             if s = "NOCHAN" then
               let get_chandef = function
                   CStr(s) -> true, s, false, ((BInt(1),BInt(1)),[[BInt(1)]])
                 | ChanMat(m) -> true, "", true, m
                 | _ -> false, "", false, ((BInt(1),BInt(1)),[[BInt(1)]])
               in
               let isvalid, cfuncstr, ismat, cmat = get_chandef ve in
               let ctype = get_calctype env1 ref.channel in
               (* Add channel to image! *)
               let env2 = image_add env1 (find_image env ref.image) ref.channel
                                    ctype isvalid ismat cfuncstr cmat
               in env2
              else
                  (raise (Failure(s)));
          )
        in envNew, ChanAssign(ref,ve)
  | LibCall(f,args) -> env, LibCall(f,args)


(*
 * VERIFY:
 *   our statement checker (invokes the expression checker)
 *)
let check_stmt env = function
    Expr(e) -> let env1, vexpr = check_expr env e in
                env1, Expr(vexpr)
  | VDecl(v) -> let env1 = var_add env v in
                env1, VDecl(v)
  | VAssign(v,op,e) ->
        (* NOTE: order is important here!
         *       the variable is not declared "in scope" until the end
         *       of the statement (i.e. you can't reference the variable
         *       from within its assignment inititalizer)
         *)
        let env1, vexpr = check_expr env e in
        let env2 = var_add env v in
        let lhs = type_of_vdecl v in
        let rhs = type_of_expr env2 vexpr in
        let env3 = check_assignment env2 rhs op lhs in
        env3, VAssign(v, op, vexpr)

(*
 * VERIFIER!
 *)
let verify program =
  let venv, vslist = (List.fold_left
    (fun (envO,slist) s -> let env, vstmt =
                             check_stmt envO s in env, vstmt :: slist)
    ( {calc = [];
       images = [];
       kernels = []}, [] ) (List.rev program))
  in
  venv, (List.rev vslist)

