(*
 * File: ast.mli
 * Author: Jeremy C. Andrus <jeremya@cs.columbia.edu>
 * Date: 2011-10-10
 *
 * PLT Fall 2011
 * Homework 1
 * Problem 3
 *)

type operator = Add | Sub | Mul | Div

type expr =
    Binop of expr * operator * expr
  | Lit of int
  | Variable of int
  | Assign of int * expr
  | Seq of expr * expr

