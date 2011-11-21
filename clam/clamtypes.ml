(*
 * File: clamtypes.ml
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
open ExtString

exception ParseErr of exn * (string * int * int * string * string)

let print_clamerr = function
    ParseErr(exn,(file,line,cnum,tok,tail)) ->
      let extra = Printexc.to_string exn in
      let fname = if file = "" then "<stdin>" else file in
      let estr =
        if tok = "" then
          Printf.sprintf "%s" extra
        else
          Printf.sprintf "%s at %s:%u:%u near \"%s%s\""
            extra fname line cnum tok (String.slice ~last:32 tail)
        in
      prerr_endline estr;
  | _ -> ()


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
  ichannels : (string * calcT) list;
}

type kernelT = {
  kname     : string;
  kchannels : (string * calcT) list;
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
                  kchannels = []; } ]
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
                      ^nm^"<"^(Backend.typestr t)^">"))
                    else hd :: add_unique_calc tl
      in
      let env1 = { env with calc = add_unique_calc env.calc } in
      env1

(* Find the type of the named variable:
 * raises a "Failure" exception if it's undefined
 *)
let type_of varname env =
        (* replace List.mem with List.exists ...) *)
    if (List.exists
         (fun c -> if c.cname = varname then true else false)
         env.calc)
    then
      let cval = List.find
         (fun c -> if c.cname = varname then true else false)
         env.calc in CalcT(varname,cval.ctype)
    else if (List.exists
              (fun i -> if i.iname = varname then true else false)
              env.images) then ImageT(varname)
    else if (List.exists
              (fun k -> if k.kname = varname then true else false)
              env.kernels) then KernelT(varname)
    else (raise (Failure("Undefined variable: "^varname)))
