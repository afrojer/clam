(*
 * File: scanner.mll
 * Date: 2011-10-11
 *
 * PLT Fall 2011
 * CLAM Project
 * Jeremy C. Andrus <jeremya@cs.columbia.edu>
 * Robert Martin <rdm2128@columbia.edu>
 * Kevin Sun <kfs2110@columbia.edu>
 * Yongxu Zhang <yz2419@columbia.edu>
 *
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

