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

let rec stmt_eval env = function
    Expr(e) -> "expr", env
  | VDecl(v) -> "vdecl", env
  | VDef(v,e) -> "vdef", env


(*
let rec eval env = function
    Lit(x) -> string_of_int(x), env
  | Variable(x) ->
      if VarMap.mem x env then
        (string_of_int(VarMap.find x env)), env
      else raise (Failure ("undeclared variable "^(string_of_int x)))
  | Binop(e1, op, e2) ->
      let v1, env = eval env e1 in
      let v2, env = eval env e2 in
      (match op with
          Add -> (v1 ^ " + " ^ v2)
        | Sub -> (v1 ^ " - " ^ v2)
        | Mul -> (v1 ^ " * " ^ v2)
        | Div -> (v1 ^ " / " ^ v2)), env
  | Assign(x, e) ->
      let v, env = eval env e in
      v, (VarMap.add x (int_of_string v) env)
  | Seq(e1, e2) ->
      let v, env = eval env e1 in
      let v, env = eval env e2 in
      v, env
*)

(* empty shell for now... replace this with actual contents... *)
let generate_c program = 
  let result, env = List.fold_left
    (fun (r,e) s -> let v, env = stmt_eval e s in v ^ r, env)
    ("", VarMap.empty) program in
      "static const char *pstr = \"" ^ result ^ "\";\n"^
      "#include <stdio.h>\n"^
      "int main(int argc, char *argv) {\n"^
      "  printf(\"Hello CLAM:\\n  output=%s\\n\", pstr);\n"^
      "}\n"

