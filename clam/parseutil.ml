(*
 * File: parseutil.ml
 * Date: 2011-11-14
 *
 * PLT Fall 2011
 * CLAM Project
 * Jeremy C. Andrus <jeremya@cs.columbia.edu>
 * Robert Martin <rdm2128@columbia.edu>
 * Kevin Sun <kfs2110@columbia.edu>
 * Yongxu Zhang <yz2419@columbia.edu>
 *
 *)

(* adapted from:
 * http://repo.or.cz/w/sqlgg.git
 *)

open Scanner

exception ParseErr of exn * (string * int * int * string * string)

let parse_buf_exn lexbuf fname =
  try
    lexbuf.Lexing.lex_curr_p <- {
        Lexing.pos_fname = fname;
        Lexing.pos_lnum = 1;
        Lexing.pos_bol = 1;
        Lexing.pos_cnum = 1;
    };
    Parser.program Scanner.token lexbuf
  with exn ->
    begin
      let curr = lexbuf.Lexing.lex_curr_p in
      let file = curr.Lexing.pos_fname in
      let line = curr.Lexing.pos_lnum in
      let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
      let tok = Lexing.lexeme lexbuf in
      let tail = Scanner.tokTail "" lexbuf in
      raise (ParseErr (exn,(file,line,cnum,tok,tail)))
    end

let parse_stdin () = parse_buf_exn (Lexing.from_channel stdin) "<stdin>"

let parse_string str = parse_buf_exn (Lexing.from_string str) "<string>"

let parse_file filename =
  let chan = Pervasives.open_in filename in
  let prog = parse_buf_exn (Lexing.from_channel chan) filename in
  Pervasives.close_in chan; prog
