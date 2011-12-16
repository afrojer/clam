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
open Environ
open Printer

(*
 * Find an image by name from the environment
 *)
let find_image env imgnm = List.find
  (fun i -> if i.iname = imgnm then true else false)
  env.images

let find_kernel env knm =
  let kern = (List.find
               (fun k -> if k.kname = knm then true else false)
               env.kernels) in
  kern.kallcalc, kern.kunusedcalc

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

let kcalc_add env kname allC unusedC =
  let kT = (List.find
            (fun k -> if k.kname = kname then true else false)
            env.kernels)
  in
    kT.kallcalc <- List.append kT.kallcalc allC;
    kT.kunusedcalc <- List.append kT.kunusedcalc unusedC;
    env

(* Set a matrix value in a CalcT *)
let calc_set_matrix env nm mat =
  let cval = (List.find
              (fun c -> if c.cname = nm then true else false)
              env.calc)
  in
    cval.cismat <- true;
    cval.cmatrix <- mat;
    env

(* Set the CStr in a CalcT *)
let calc_set_cfunc env nm func =
  let cval = (List.find
              (fun c -> if c.cname = nm then true else false)
              env.calc)
  in
    cval.cisvalid <- true;
    cval.cismat <- false;
    cval.cfunc <- func;
    env

let calc_ismat env nm =
  let cval = (List.find
              (fun c -> if c.cname = nm then true else false)
              env.calc)
  in cval.cismat

let calc_isvalid env nm =
  let cval = (List.find
              (fun c -> if c.cname = nm then true else false)
              env.calc)
  in cval.cisvalid

(*
 * Add a channel to the specified image
 * (the caller needs to initialize most of the member variables)
 *)
let image_add env img channel typ isvalid ismat cfuncstr cmat =
  (* add the channel, to the image *)
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
 *   a convolution statement
 *   i.e. it checks validity of "Channel ** Kernel" statements
 *        and retuns a list of new channels produced by the
 *        convolution (so we can add them to the resulting image)
 *)
let check_convolve env chanref kernelref =
  let get_kcalc = function
                  KernCalc(k) -> k.allcalc, k.unusedcalc
                | Id(i) -> find_kernel env i
                | _ -> (raise (Failure("Invalid convolution kernel!"))) in
  let allC, unusedC = get_kcalc kernelref in
  let calc_is_used chname =
        if (List.exists
            (fun nm -> if nm = chname then true else false)
            unusedC)
        then false else true in
  let chanlist = (List.fold_left
                  (fun lst calc ->
                          if (calc_is_used calc) then calc :: lst else lst)
                  [] allC) in
  env, chanlist

(*
 * VERIFY:
 *   our basic type checker!
 *   checks for a valid assignment from rhs to lhs
 *   i.e. it checks the expression:
 *              lhs = rhs
 *        for validity
 *)
let check_assignment env rhs rhse op = function (* passes in LHS *)
    ImageT(nm) ->
        let optype rhs = function
            DefEq -> (raise (Failure("Cannot define "^
                                     (string_of_vdecl (ImageT(nm)))^
                                     " with ':='")))
          | Eq ->
                let chk_img_assign = function
                    ConvT(a,b) ->
                        let env1, chanlst = check_convolve env a b in
                        let img = find_image env1 nm in
                        let envNew = List.fold_left
                            (fun e c -> image_add e img c (get_calctype e c)
                                        false false ("",[])
                                        ((BInt(1),BInt(1)),[[BInt(1)]]) )
                            env1 chanlst in
                        envNew
                  | ImageT(nm) -> env
                  | _ as t -> (raise (Failure("Can't assign '"^
                                         (string_of_vdecl t)^
                                         "' to "^(string_of_vdecl (ImageT(nm)))^
                                         ": Image = Image; only!")))
                in
                let env1 = chk_img_assign rhs in
                env1
          | OrEq ->
                let chk_chan_add = function
                    CalcT(cnm,t) ->
                        let isvalid = calc_isvalid env cnm in
                        if not isvalid then
                          (raise (Failure("Can't assign an un-initialized "^
                                          "calculation '"^
                                          cnm^ "' as an image channel")))
                        else
                          let ismat = calc_ismat env cnm in
                          if ismat then
                            (raise (Failure("Can't assign a matrix "^
                                            "calculation '"^
                                            cnm^"' to an image channel")))
                          else cnm, t
                  | _ as t -> (raise (Failure("Can't assign "^
                               (string_of_vdecl t)^" to "^
                               (string_of_vdecl (ImageT(nm)))^
                               ": Invalid image channel in |=")))
                in
                let chname, chtype = chk_chan_add rhs in
                let env1 = image_add env (find_image env nm) chname chtype
                                     false false ("",[])
                                     ((BInt(1),BInt(1)),[[BInt(1)]]) in
                env1
        in optype rhs op
  | KernelT(nm) ->
        let kcalc = function
              KCalcT(k) -> k
            | _ -> (raise (Failure("Internal Error."))) in
        let optype rhs = function
            DefEq -> (raise (Failure("Cannot define "^
                                     (string_of_vdecl (KernelT(nm)))^
                                     " with ':='")))
          | Eq -> if not (rhs = KCalcT(kcalc rhs))
                  then (raise (Failure("Can't assign "^(string_of_vdecl rhs)^
                                       " to "^(string_of_vdecl (KernelT(nm)))^
                                       ": Kernel = Kernel only!")))
                  else
                    let kc = kcalc rhs in
                    let env1 = kcalc_add env nm kc.allcalc kc.unusedcalc in
                    env1
          | OrEq -> let chk_calc_add = function
                          CalcT(cnm,t) -> [cnm], []
                        | KCalcT(k) -> k.allcalc, k.unusedcalc
                        | _ as t -> (raise (Failure("Can't add "^
                                             (string_of_vdecl t)^" to "^
                                             (string_of_vdecl (KernelT(nm)))^
                                             ": Invalid CalcT!")))
                    in
                    let allC, unusedC = chk_calc_add rhs in
                    let env1 = kcalc_add env nm allC unusedC in
                    env1
        in optype rhs op
  | CalcT(nm,t) ->
        let optype rhs = function
            DefEq ->
                let update_calc = function
                  | ChanMat(m) -> calc_set_matrix env nm m
                  | CStr(s,idl) -> calc_set_cfunc env nm (s,idl)
                  | _ as e -> (raise (Failure("Cannot define "^
                                              (string_of_vdecl (CalcT(nm,t)))^
                                              " with '"^
                                              (string_of_vdecl (type_of_expr env e))^"'")))
                in
                let env1 = update_calc rhse in
                env1
          | _ as op -> (raise (Failure("Cannot define "^
                                      (string_of_vdecl (CalcT(nm,t)))^
                                       " with '"^(string_of_op op)^"'")))
        in optype rhs op
  | ConvT(_,_) | KCalcT(_) ->
        (raise (Failure("Can't assign to internal type!")))
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
 *   our expression checker
 *)
let rec check_expr env = function
    Id(i) -> let _ = type_of env i in env, Id(i)
  | Integer(BInt(i)) -> env, Integer(BInt(i))
  | LitStr(s) -> env, LitStr(s)
  | CStr(s,idl) -> env, CStr(s,idl)
  | KernCalc(k) -> env, KernCalc(k)
  | ChanMat(m) -> let denom = (snd (fst m)) in
                  let matrix = snd m in
                  if (denom <> BInt(0)) then
                     let eqrows = List.fold_left
                                    (fun cols lst -> if List.length lst = cols
                                       then List.length lst else -1)
                                    (List.length (List.hd matrix)) matrix
                     in
                     if eqrows = -1 then raise (Failure("Unequal matrix rows"))
                     else env, ChanMat(m)
                  else raise(Failure("Divide by zero"))
  | ChanRef(c) -> check_chanref env c false; env, ChanRef(c)
  | Convolve(a,b) ->
        let env1, va = check_expr env a in
        let env2, vb = check_expr env1 b in
        env2, Convolve(va,vb)
  | Assign(i,op,v) ->
        let lhs = type_of env i in
        let env1, vexpr = check_expr env v in
        let rhs = type_of_expr env1 vexpr in
        let env2 = check_assignment env1 rhs vexpr op lhs in
        env2, Assign(i,op,vexpr)
  | ChanAssign(ref,v) ->
        let env1, ve = check_expr env v in
        let envNew =
          (try check_chanref env1 ref true; env
           with Failure(s) ->
             if s = "NOCHAN" then
               let get_channame = function
                     CStr(_) | ChanMat(_) -> ref.channel
                   | ChanRef(c) -> c.channel
                   | _ as t -> (raise (Failure("Cannot assign "^
                                               (string_of_vdecl (type_of_expr
                                               env1 t))^" to "^
                                               ref.image^":"^ref.channel)))
               in
               let get_chandef = function
                   CStr(s,idl) -> true, (s,idl), false, ((BInt(1),BInt(1)),[[BInt(1)]])
                 | ChanMat(m) -> true, ("",[]), true, m
                 | _ -> false, ("",[]), false, ((BInt(1),BInt(1)),[[BInt(1)]])
               in
               let channame = get_channame ve in
               let isvalid, cfuncstr, ismat, cmat = get_chandef ve in
               let ctype = get_calctype env1 channame in
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
    Expr(e) -> print_endline ("Expr...");
        let env1, vexpr = check_expr env e in
          env1, Expr(vexpr)
  | VDecl(v) -> print_endline ("VDecl...");
        let env1 = var_add env v in
          env1, VDecl(v)
  | VAssign(v,op,e) ->
        print_endline ("VAssign...");
        (* NOTE: order is important here!
         *       the variable is not declared "in scope" until the end
         *       of the statement (i.e. you can't reference the variable
         *       from within its assignment inititalizer)
         *)
        let env1, vexpr = check_expr env e in
        let env2 = var_add env v in
        let lhs = v in
        let rhs = type_of_expr env2 vexpr in
        let env3 = check_assignment env2 rhs vexpr op lhs in
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
       kernels = []}, [] ) program)
  in
  venv, vslist

