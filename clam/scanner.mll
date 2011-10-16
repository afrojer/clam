(*
 * File: scanner.mll
 * Author: Jeremy C. Andrus <jeremya@cs.columbia.edu>
 * Date: 2011-10-10
 *
 * PLT Fall 2011
 * Homework 1
 * Problem 3
 *)

{ open Parser }
rule token =
  parse [' ' '\t' '\r' '\n'] { token lexbuf }
    | '+'               { PLUS }
    | '-'               { MINUS }
    | '*'               { TIMES }
    | '/'               { DIVIDE }
    | ','               { SEQ }
    | '='               { ASSIGN }
    | '$'['0'-'9'] as lit { VARIABLE(int_of_char lit.[1] - 48) }
    | ['0'-'9']+ as lit { LITERAL(int_of_string lit) }
    | eof               {EOF}

