(*
 * File: backend.ml
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

open String
open Ast
open Sast


let fold_c c_of_x =
  List.fold_left (fun src x -> src ^ (c_of_x x)) ""


let c_of_scope scope =
  "/* TODO: C for Declaration of stuff we collected from the environment */\n"
  

let c_of_aItem = function
    Debug(s) -> "/* DEBUG: " ^ s ^ " */\n"
  | _ -> "/* ACTION ITEM */\n"

let generate_c scope vast =
  "/* Generated Environment C */\n" ^
  (c_of_scope scope) ^
  "/* Generated Syntax Tree C */\n" ^
  "int main(int argc, char **argv) {\n" ^
  (fold_c c_of_aItem vast) ^
  "  return 0;\n" ^
  "}\n"


(*
(* NOTE: We are not required to declare Calc, Kernel, or Image
 * in the same place that the CLAM programmer did it. This
 * section can do nothing as long as these variables are
 * listed in the 'env' variable and can therefore be
 * declared as global variables in the C source...         *)
let c_of_vdecl v = match v with
    ImageT(id)     -> "/* Declare ImageT: "^id^", */\n"
  | KernelT(id)    -> "/* Declare KernelT "^id^" */\n"
  | CalcT(id,atom) -> "/* Declare CalcT "^id^" of type "^(Printer.string_of_atom atom)^" */\n"
  | ConvT(_,_) | KCalcT(_) | StrT(_,_) | BareT(_) ->
        raise (Failure("Internal error [internal type used in vdecl)"))

let c_of_convolve e1 e2 =
  let ch = match e1 with
      ChanRef(ch) -> ch
    | _ -> (raise(Failure("Error: cannot convolve without a channel reference")))
  in
  let kc = match e2 with
      KernCalc(kc) -> kc
    | _ -> (raise(Failure("Error: cannot convolve without a Kernel calculation")))
  in
  "printf(\"Convolve:\\n\");\n" ^
  "printf(\"    " ^ ch.image ^ "." ^ ch.channel ^ " -> {" ^
  (
    List.fold_left
    (
      fun s1 s2 -> (s1 ^ " " ^ s2)
    ) "" kc.allcalc
  ) ^ " }\\n\");\n"

let c_of_libf libf elist =
  match libf with
      ImgRead -> ("printf(\"ImgRead: \\n\");\n")
    | ImgWrite -> ("printf(\"ImgWrite: \\n\");\n")

let c_of_chanAssign ch e =
  "/* Not sure the difference between assign and chanAssign.... */\n"

let c_of_eq id e =
  let rhs = match e with
      ChanRef(ch) -> ("<ChanRef " ^ ch.image ^ "." ^ ch.channel ^ ">")
    | Id(s) -> ("<Id " ^ s ^ ">")
    | KernCalc(kc) -> ("<KernCalc>")
    | _ -> "<Unknown>" (*(raise (Failure("Only can assign using '=' operator for image channel <- channel, or image <- image, or image <- kerncalc")))*)
  in
  "printf(\"" ^ id ^ " = " ^ rhs ^ "\\n\");\n"

(* OrEquals should only be used to add a channel to an image from a CalcT *)
let c_of_oreq id e =
  let tmp = match e with
      ChanRef(ch) -> ("OrEq <ChanRef " ^ ch.image ^ "." ^ ch.channel ^ ">")
    | Id(s) -> ("<Identifier " ^ s ^ ">")
    | _ -> (raise (Failure("Cannot assign non channel using |= operator")))
  in
  "printf(\"" ^ escaped(tmp) ^ "\\n\");\n"

(* DefEq implies that id is a calc *)
let c_of_defeq id e =
  let calc_source = match e with
      ChanMat(cm) -> "Matrix"
    | CStr(st, ids) -> (List.fold_left (fun s1 s2 -> s1 ^ " " ^ s2) "CStr containing:" ids)
    | _ -> (raise (Failure("Cannot use := operator unless assigning a channel")))
  in
  "printf(\"" ^ escaped(calc_source) ^ "\\n\");\n"


  


let c_of_assign id op e =
  match op with
         Eq -> ("printf(\"Equality assignment op: \");\n" ^ (c_of_eq id e))
    |  OrEq -> ("printf(\"OrEquals assignment op: \");\n" ^ (c_of_oreq id e))
    | DefEq -> ("printf(\"DefEquals assignment op: \");\n" ^ (c_of_defeq id e))

let c_of_expr expr = match expr with
    Id(s) -> (raise (Failure("Not sure how to convert identifier '"^s^"' to C source")))
  | Integer(bi) -> (match bi with BInt(i) -> string_of_int(i))
  | LitStr(s) -> ("\"" ^ escaped(s) ^ "\"")
  | CStr(s,slist) -> (raise (Failure("Not sure how to convert CStr to C source out of context")))
  | KernCalc(kc) -> (raise (Failure("Not sure how to convert Kernel Calculation to C source out of context")))
  | ChanMat(cm) -> (raise (Failure("Not sure how to convert Matrix channel to C source out of context")))
  | ChanRef(ch) -> (raise (Failure("Not sure how to convert ChanRef to C source out of context")))
  | Convolve(e1, e2) -> (c_of_convolve e1 e2)
  | Assign(id,op,e) -> (c_of_assign id op e)
  | ChanAssign(ch, e) -> (c_of_chanAssign ch e)
  | LibCall(libf,elist) -> (c_of_libf libf elist)

(* TODO: VAssign's are of the form (vdecl,op,e) but Assign's (the expression
 * are of the form (string,op,e). We should have a consistent representation
 * because really a VAssign should be equivalent to VDecl + Assign. Right
 * now there's no way to convert directly from (VDecl -> string) because some
 * VDecl types (KCalcT, ConvT, BareT) don't have string identifiers *)

let c_of_stmt stmt =
  match stmt with
      Expr(e) -> (c_of_expr e)
    | VDecl(v) -> (c_of_vdecl v)
    | VAssign(v,op,e) -> (c_of_vdecl v) ^ (c_of_assign "Identifier TODO -- see backend.ml" op e)



(*
 * Splicing Code Together
 *)

let generate_preamble_c =
  "#include <stdio.h>\n" ^
  "/* Dump the contents of our clam.h file here */\n"

let generate_declarations_c env =
  "/* Generate declarations for our variables here, using the environment\n" ^
  " * variable (env). This will probable include:\n" ^
  " *   - Calc definition (using #define)\n" ^
  " *   - Global variables corresponding to ImageT pointers\n" ^
  " *   - Global variables corresponding to KernelT pointers\n"

let generate_main_c ast =
  "/* Strategy: Map each statement in the AST to its corresponding C source code.\n" ^
  " *           Then, append this text together (using ^ aka strcat).\n" ^
  " */\n" ^
  (List.fold_left (^) "" (List.map c_of_stmt ast))

let generate_c env ast =
  let c_source =
    "\n/* CLAM: PREAMBLE */\n" ^
    (generate_preamble_c)  ^
    "\n/* CLAM: DEFINITIONS */\n" ^
    (generate_declarations_c env) ^
    "\n/* CLAM: MAIN */\n" ^
    "int main(int argc, char *argv) {\n" ^
    (generate_main_c ast) ^
    "\n" ^
    "  return 0;\n" ^
    "}\n"
  in
  c_source
*)
