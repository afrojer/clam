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
      "("^i^(Printer.string_of_op op)^e2^")\n", env
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
  | CalcT(nm,typ) -> nm^"<"^(Printer.string_of_type typ)^">", env

let stmt_eval env = function
    Expr(e) -> let s, env = expr_eval env e in
      "expr:"^s^";\n", env
  | VDecl(v) -> let vstr, env = vdecl_eval env v in
      "vdecl:"^vstr^";\n", env
  | VAssign(v,op,e) -> let vstr, env = vdecl_eval env v in
      let ops = Printer.string_of_op op in
      let ex, env = expr_eval env e in
      "vassign:" ^ vstr ^ ops ^ ex, env



let generate_preamble_c env ast =
  "#include <stdio.h>\n"

let generate_definitions_c env ast =
  "char *ast = \"" ^ (String.escaped (Printer.string_of_ast ast)) ^ "\";\n"

let generate_main_c env ast =
  "int main(int argc, char *argv) {\n" ^
  "  printf(\"The AST that generated this binary was:\\n%s\\n\", ast);\n" ^
  "  return 0;\n" ^
  "}\n"



let generate_c env ast =
  let c_source =
    "\n/* CLAM: PREAMBLE */\n" ^
    (generate_preamble_c env ast) ^
    "\n/* CLAM: DEFINITIONS */\n" ^
    (generate_definitions_c env ast) ^
    "\n/* CLAM: MAIN */\n" ^
    (generate_main_c env ast)
  in
  c_source


(*
  let result, env = List.fold_left
    (fun (r,envO) stmt -> let v, env = stmt_eval envO stmt in v ^ r, env)
    ("", verifier_env) program in
      "static const char *pstr = \"" ^ (String.escaped result) ^ "\";\n"^
      "#include <stdio.h>\n"^
      "int main(int argc, char *argv) {\n"^
      "  printf(\"Hello CLAM:\\n  output=%s\\n\", pstr);\n"^
      "}\n"
*)

