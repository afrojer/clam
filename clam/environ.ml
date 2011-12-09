(*
 * File: environ.ml
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
  cisvalid : bool;
  cismat   : bool; (* if true, use 'matrix' else use 'cfunc' *)
  cfunc    : string;
  cmatrix  : Ast.matrix;
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
  calc    : calcT list;
  images  : imgT list;
  kernels : kernelT list;
}



(* Add a variable definition to the environment:
 * raises a "Failure" exception if the name isn't unique
 *)
let rec var_add env = function
    ImageT(nm) -> let rec add_unique_img = function
        [] -> [ { iname = nm;
                  (* XXX: add default channels here! *)
                  ichannels = []; } ]
      | hd :: tl -> if hd.iname = nm then
                      raise (Failure("ImageT redefined: "^nm))
                    else hd :: add_unique_img tl
      in
      let env1 = { env with images = add_unique_img env.images } in
      env1
  | KernelT(nm) -> let rec add_unique_kernel = function
        [] -> [ { kname = nm;
                  kallcalc = []; kunusedcalc = []; } ]
      | hd :: tl -> if hd.kname = nm then
                      raise (Failure("KernelT redefined: "^nm))
                    else hd :: add_unique_kernel tl
      in
      let env1 = { env with kernels = add_unique_kernel env.kernels } in
      env1
  | CalcT(nm,t) -> let rec add_unique_calc = function
       [] -> [ { cname = nm;
                 ctype = t;
                 cisvalid = false;
                 cismat = false;
                 cfunc = "";
                 cmatrix = (BInt(1),BInt(1)),[[BInt(1)]];} ]
      | hd :: tl -> if hd.cname = nm then
                      raise (Failure("CalcT redefined: "
                      ^nm^"<"^(Printer.string_of_atom t)^">"))
                    else hd :: add_unique_calc tl
      in
      let env1 = { env with calc = add_unique_calc env.calc } in
      env1
  | ConvT(_,_) | KCalcT(_) | StrT(_,_) | BareT(_) -> env

(* Find the type of the named variable:
 * raises a "Failure" exception if it's undefined
 *)
let type_of env varname =
    if (List.exists
         (fun c -> if c.cname = varname then true else false)
         env.calc)
    then
      let cval = List.find
         (fun c -> if c.cname = varname then true else false)
         env.calc in CalcT(varname, cval.ctype)
    else if (List.exists
              (fun i -> if i.iname = varname then true else false)
              env.images) then ImageT(varname)
    else if (List.exists
              (fun k -> if k.kname = varname then true else false)
              env.kernels) then KernelT(varname)
    else (raise (Failure("Undefined variable: "^varname)))

(* Find the type of an expression i.e. the expression _results_ in this type *)
let rec type_of_expr env = function
    Id(i) -> type_of env i
  | Integer(BInt(i)) -> BareT("INT")
  | LitStr(s) -> StrT(":litstr", s)
  | CStr(s) -> StrT(":cstr", s)
  | KernCalc(k) -> KCalcT(k)
  | ChanEval(c) -> CalcT(c.channel, Uint8)
  | ChanMat(m) -> CalcT(":c", Uint8)
  | ChanRef(c) -> CalcT(c.channel, Uint8)
  | Convolve(a,b) -> ConvT(a,b)
  | Assign(i,op,v) -> type_of_expr env v
  | ChanAssign(ref,v) -> type_of_expr env v
  | LibCall(f,args) ->
        let ctype = function
            ImgRead -> ImageT(":i")
          | ImgWrite -> BareT("VOID") in
        ctype f

(* Find the type of a variable declaration *)
let type_of_vdecl = function
    ImageT(nm) -> ImageT(nm)
  | KernelT(nm) -> KernelT(nm)
  | KCalcT(k) -> KCalcT(k)
  | CalcT(nm,t) -> CalcT(nm, t)
  | StrT(t, s) -> StrT(t, s)
  | BareT(s) -> BareT(s)
  | ConvT(a,b) -> ConvT(a,b)

