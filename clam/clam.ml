(*
 * File: clam.ml
 * Date: 2011-10-16
 *
 * PLT Fall 2011
 * CLAM Project
 * Jeremy C. Andrus <jeremya@cs.columbia.edu>
 * Robert Martin <rdm2128@columbia.edu>
 * Kevin Sun <kfs2110@columbia.edu>
 * Yongxu Zhang <yz2419@columbia.edu>
 *
 * TODO: move the bulk of this code into something like
 *       a "backend" file...
 *)

open Ast

module VarMap = Map.Make(struct
  type t = int
  let compare x y = Pervasives.compare x y
end)

let rec eval env = function
    Lit(x) -> x, env
  | Variable(x) ->
      if VarMap.mem x env then
        (VarMap.find x env), env
      else raise (Failure ("undeclared variable "^(string_of_int x)))
  | Binop(e1, op, e2) ->
      let v1, env = eval env e1 in
      let v2, env = eval env e2 in
      (match op with
          Add -> v1 + v2
        | Sub -> v1 - v2
        | Mul -> v1 * v2
        | Div -> v1 / v2), env
  | Assign(x, e) ->
      let v, env = eval env e in
      v, (VarMap.add x v env)
  | Seq(e1, e2) ->
      let v, env = eval env e1 in
      let v, env = eval env e2 in
      v, env

(* This should be about what's left in the main file... *)
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.expr Scanner.token lexbuf in
  let result, env = eval VarMap.empty expr in
  print_endline (string_of_int result)

