(*
 * File: printer.ml
 * Date: 2011-12-02
 *
 * PLT Fall 2011
 * CLAM Project
 * Jeremy C. Andrus <jeremya@cs.columbia.edu>
 * Robert Martin <rdm2128@columbia.edu>
 * Kevin Sun <kfs2110@columbia.edu>
 * Yongxu Zhang <yz2419@columbia.edu>
 *
 *)

open ExtString
open Ast
open Envtypes
open Sast

(*
 * Strings that represent CLAM things
 *)
let string_of_op = function
    Eq -> "="
  | OrEq -> "|="
  | DefEq -> ":="

let string_of_atom = function
    Uint8 -> "U8"
  | Uint16 -> "U16"
  | Uint32 -> "U32"
  | Int8 -> "I8"
  | Int16 -> "I16"
  | Int32 -> "I32"
  | Angle -> "Angle"
  | Unknown -> "Unknown"

let string_of_libf = function
    ImgRead -> "ImgRead"
  | ImgWrite -> "ImgWrite"

let string_of_vdecl = function
    ImageT(nm) -> "Image("^nm^")"
  | KernelT(nm) -> "KernelT("^nm^")"
  | KCalcT(k) -> "KCalc"
  | CalcT(nm,t) -> "CalcT("^nm^")"
  | StrT(t, s) -> t^":"^s
  | BareT(s) -> s
  | ConvT(a,b) -> "Convolution"


let string_of_type = function
    CalcType(t) -> "Calc<" ^ (string_of_atom t) ^ ">"
  | KernelType -> "Kernel"
  | ImageType -> "Image"
  | ChanRefType -> "ChanRef"
  | FilenameType -> "Filename"
  | FormatType -> "Image Format"
  | VoidType -> "Void"

let string_of_chan ch =
  ch.image ^ "." ^ ch.channel

(*
 * Printing CLAM compiler messages
 *)
let print_clamerr = function
    Parse_util.ParseErr(exn,(file,line,cnum,tok,tail)) ->
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

(*
 * Environment Printing
 *)
let string_of_scope scope =
  let _ = scope.venv in
    "Environment:\n" ^
    "  Printer not implemented\n"

let print_scope scope =
  print_endline (string_of_scope scope)

(*
 * CLAM AST Printing
 *)
type 'a ptree = Node of 'a * ('a ptree list)

let tree_of_atom a =
  Node("Atomic Type '" ^ (string_of_atom a) ^ "'", [])

let tree_of_assign_op op =
  Node("Assignment Op '" ^ (string_of_op op) ^ "'", [])

let tree_of_bareint bi =
  match bi with
    BInt(i) -> Node("BareInt: " ^ string_of_int(i), [])

let tree_of_ident id =
  Node("Identifer '" ^ id ^ "'", [])

let tree_of_kerncalc kc =
  Node("? KernCalc", [])

let tree_of_chanref ref =
  Node("? ChanRef", [])

let tree_of_chanmat mat =
  Node("? ChanMat", [])

let tree_of_libf libf =
  Node("Library Function: " ^ (string_of_libf libf), [])
  
let tree_of_vdecl vdecl =
  match vdecl with
      ImageT(id) -> Node("Variable Declaration [Image Type]", [tree_of_ident id])
    | KernelT(id) -> Node("Variable Declaration [Kernel Type]", [tree_of_ident id])
    | CalcT(id, a) -> Node("Variable Declaration [Calc Type]", [tree_of_ident id; tree_of_atom a])
    | KCalcT(k) -> Node("INVALID use of KCalcT", [])
    | _ -> Node("Invalid use of ...", [])

let rec tree_of_expr expr =
  let tupl = match expr with
      Id(id) -> ("Identifier", [tree_of_ident id])
    | Integer(bi) -> ("Integer", [tree_of_bareint bi])
    | LitStr(s) -> ("LitStr", [Node("'" ^ s ^ "'", [])])
    | CStr(s,idl) -> ("CStr", [Node("'" ^ s ^ "'", [])])
    | KernCalc(kc) -> ("KernCalc", [tree_of_kerncalc kc])
    | ChanMat(mat) -> ("ChanMat", [tree_of_chanmat mat])
    | ChanRef(ref) -> ("ChanRef", [tree_of_chanref ref])
    | Convolve(e1, e2) -> ("Convolve", [tree_of_expr e1; tree_of_expr e2])
    | Assign(id, op, e) -> ("Assign", [tree_of_ident id; tree_of_assign_op op; tree_of_expr e])
    | ChanAssign(ref, e) -> ("ChanAssign", [tree_of_chanref ref; tree_of_expr e])
    | LibCall(libf, elist) -> ("LibCall", List.append [tree_of_libf libf] (List.map tree_of_expr elist))
  in
  Node("Expression [" ^ fst(tupl) ^ "]", snd(tupl))

let tree_of_stmt stmt =
  let children = match stmt with
      Expr(e) -> [tree_of_expr e]
    | VDecl(v) -> [tree_of_vdecl v]
    | VAssign(v, op, e) -> [tree_of_vdecl v; tree_of_assign_op op; tree_of_expr e]
  in
  Node("Statement", children)

let tree_of_ast ast =
  let children = List.map tree_of_stmt ast in
  Node("Abstract Syntax Tree", children)

let rec string_of_tree prefix = function Node(str, ch) ->
  let string_of_children =
    List.fold_left (^) "" (List.map (string_of_tree (prefix ^ "    ")) ch)
  in
  prefix ^ " * " ^ str ^ "\n" ^ string_of_children

let string_of_ast ast =
  string_of_tree "" (tree_of_ast ast)


