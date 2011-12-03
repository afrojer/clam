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

open Ast
open Clamtypes


type 'a ptree = Node of 'a * ('a ptree list)

let tree_of_atom a =
  let str = match a with
      Uint8  -> "Uint8"
    | Uint16 -> "Uint16"
    | Uint32 -> "Uint32"
    | Int8   -> "Int8"
    | Int16  -> "Int16"
    | Int32  -> "Int32"
    | Angle  -> "Angle"
  in
  Node("Atomic '" ^ str ^ "'", [])

let tree_of_assign_op op =
  let str = match op with
      Eq    -> "Eq"
    | OrEq  -> "OrEq"
    | DefEq -> "DefEq"
  in
  Node("Assignment Op '" ^ str ^ "'", [])

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
  let str = match libf with
      ImgRead -> "ImgRead"
    | ImgWrite -> "ImgWrite"
  in
  Node("Library Function: " ^ str, [])
  
let tree_of_vdecl vdecl =
  match vdecl with
      ImageT(id) -> Node("Variable Declaration [Image Type]", [tree_of_ident id])
    | KernelT(id) -> Node("Variable Declaration [Kernel Type]", [tree_of_ident id])
    | CalcT(id, a) -> Node("Variable Declaration [Calc Type]", [tree_of_ident id; tree_of_atom a])

let rec tree_of_expr expr =
  let tupl = match expr with
      Id(id) -> ("Identifier", [tree_of_ident id])
    | Integer(bi) -> ("Integer", [tree_of_bareint bi])
    | LitStr(s) -> ("LitStr", [Node("'" ^ s ^ "'", [])])
    | CStr(s) -> ("CStr", [Node("'" ^ s ^ "'", [])])
    | KernCalc(kc) -> ("KernCalc", [tree_of_kerncalc kc])
    | ChanEval(ref) -> ("ChanEval", [tree_of_chanref ref])
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

let rec print_tree prefix tree =
  match tree with
    Node(str, ch) -> print_endline(prefix ^ " * " ^ str); List.iter (print_tree (prefix ^ "    ")) ch

let print_ast ast =
  print_tree "" (tree_of_ast ast)

