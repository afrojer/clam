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
 *)

open Ast
open Envtypes

let default_matrix () = (BInt(1),BInt(1)),[[BInt(1)]]

let default_image nm =
  { iname = nm;
    ichannels =
    [
      "Red",
      { cname = "Red";
        ctype = Uint8;
        cisvalid = true;
        cismat = false;
        cfunc = "",[];
        cmatrix = default_matrix (); };
      "Green",
      { cname = "Green";
        ctype = Uint8;
        cisvalid = true;
        cismat = false;
        cfunc = "",[];
        cmatrix = default_matrix (); };
      "Blue",
      { cname = "Blue";
        ctype = Uint8;
        cisvalid = true;
        cismat = false;
        cfunc = "",[];
        cmatrix = default_matrix (); };
    ];
  }

(* Add a variable definition to the environment:
 * raises a "Failure" exception if the name isn't unique
 *)
let do_add_ident env vd =
  let add_unique_var id =
        if (List.exists (fun c -> c = id) env.allvars)
        then (raise (Failure("variable re-defined: "^id)))
        else env.allvars <- id :: env.allvars
  in
  match vd with
    ImageT(nm) ->  add_unique_var nm
  | KernelT(nm) -> add_unique_var nm
  | CalcT(nm,t) -> add_unique_var nm
  | _ -> ()

(* add a strongly-typed variable (for easier/faster searching) *)
let rec var_add env vd = do_add_ident env vd;
  match vd with
    ImageT(nm) -> let rec add_unique_img = function
        [] -> [ default_image nm ]
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
                 cfunc = "",[];
                 cmatrix = default_matrix ();} ]
      | hd :: tl -> if hd.cname = nm then
                      raise (Failure("CalcT redefined: "
                      ^nm^"<"^(Printer.string_of_atom t)^">"))
                    else hd :: add_unique_calc tl
      in
      let env1 = { env with calc = add_unique_calc env.calc } in
      env1
  | ConvT(_,_) | KCalcT(_) | StrT(_,_) | BareT(_) -> env

let imgt_of_id env name =
  List.find (fun i -> i.iname = name) env.images

let calct_of_id env name =
  List.find (fun c -> c.cname = name) env.calc

let kernt_of_id env name =
  List.find (fun k -> k.kname = name) env.kernels

(* Find the type of the named variable:
 * raises a "Failure" exception if it's undefined
 *)
let type_of env varname =
    if (List.exists
         (fun c -> if c.cname = varname then true else false)
         env.calc)
    then
      let cval = calct_of_id env varname in CalcT(varname, cval.ctype)
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
  | CStr(s,idl) -> StrT(":cstr", s)
  | KernCalc(k) -> KCalcT(k)
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

