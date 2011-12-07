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

open Ast

module VarMap = Map.Make(struct
  type t = int
  let compare x y = Pervasives.compare x y
end)

let opstr = function
    Eq -> "="
  | OrEq -> "|="
  | DefEq -> ":="

let atomstr = function
    Uint8 -> "U8"
  | Uint16 -> "U16"
  | Uint32 -> "U32"
  | Int8 -> "I8"
  | Int16 -> "I16"
  | Int32 -> "I32"
  | Angle -> "Angle"

let rec expr_eval env = function
    Id(i) -> i, env
  | Integer(BInt(i)) -> Printf.sprintf "%i" i, env
  | LitStr(s) -> "\""^s^"\"", env
  | CStr(s) -> "inline foo() { "^s^" }\n", env
  | KernCalc(k) -> "[kerncalc]\n", env
  | ChanEval(c) -> "$(chaneval)", env
  | ChanMat(m) -> "[matrix]\n", env
  | ChanRef(c) ->  "[chanref]\n", env
  | Convolve(a,b) ->
      let e1, env = expr_eval env a in
      let e2, env = expr_eval env b in
      "("^e1^"**"^e2^")", env
  | Assign(i,op,v) ->
      let e2, env = expr_eval env v in
      "("^i^(opstr op)^e2^")\n", env
  | ChanAssign(ref,v) -> "[chanassign]\n", env
  | LibCall(f,args) -> let av, aenv =
      (List.fold_left (fun (r,envO) s ->
                        let v, env = expr_eval envO s in
                        v^","^r, env)
                      ("", env) args) in
        "libcall("^av^")\n", aenv

let vdecl_eval env = function
    ImageT(img) -> img, env
  | KernelT(k) -> k, env
  | CalcT(nm,typ) -> nm^"<"^(atomstr typ)^">", env
  | StrT(t,s) -> "", env
  | BareT(s) -> "", env (* only used for type checking... *)

let stmt_eval env = function
    Expr(e) -> let s, env = expr_eval env e in
      "expr:"^s^";\n", env
  | VDecl(v) -> let vstr, env = vdecl_eval env v in
      "vdecl:"^vstr^";\n", env
  | VAssign(v,op,e) -> let vstr, env = vdecl_eval env v in
      let ops = opstr op in
      let ex, env = expr_eval env e in
      "vassign:" ^ vstr ^ ops ^ ex, env

(* empty shell for now... replace this with actual contents... *)
let generate_c verifier_env program =
  let result, env = List.fold_left
    (fun (r,envO) stmt -> let v, env = stmt_eval envO stmt in v ^ r, env)
    ("", verifier_env) program in
      "static const char *pstr = \"" ^ (String.escaped result) ^ "\";\n"^
      "#include <stdio.h>\n"^
      "int main(int argc, char *argv) {\n"^
      "  printf(\"Hello CLAM:\\n  output=%s\\n\", pstr);\n"^
      "}\n"

