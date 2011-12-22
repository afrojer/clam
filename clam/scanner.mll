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
{ open Parser

exception LexError of string

(* string parsing from OCaml compiler code :-) *)
let string_buff = Buffer.create 256
let reset_string_buffer () = Buffer.clear string_buff
let store_string_char c = Buffer.add_char string_buff c
let store_string_snip str = Buffer.add_string string_buff str
let get_stored_string () = Buffer.contents string_buff

(* ID list for 'escaped C string' parsing *)
let idlist = ref []
let reset_idlist () = idlist := []
let add_id_to_list id =
  if not (List.exists (fun i -> if i = id then true else false) !idlist) then
    idlist := id :: !idlist
  else ()

let char_for_backslash = function
    'n' -> '\n'
  | 't' -> '\t'
  | 'b' -> '\b'
  | 'r' -> '\r'
  | c   -> c

let decimal_code c d u =
  100 * (Char.code c - 48) + 10 * (Char.code d - 48) + (Char.code u - 48)

let char_for_hexadecimal_code d u =
  let d1 = Char.code d in
  let val1 = if d1 >= 97 then d1 - 87
             else if d1 >= 65 then d1 - 55
             else d1 - 48
  in
  let d2 = Char.code u in
  let val2 = if d2 >= 97 then d2 - 87
             else if d2 >= 65 then d2 - 55
             else d2 - 48
  in
  Char.chr (val1 * 16 + val2)

let lex_warning lexbuf msg =
  let p = Lexing.lexeme_start_p lexbuf in
  Printf.eprintf "CLAM warning:\nFile \"%s\", line %d, character %d: %s.\n"
    p.Lexing.pos_fname p.Lexing.pos_lnum
    (p.Lexing.pos_cnum - p.Lexing.pos_bol + 1) msg;
  flush stderr

let incr_loc lexbuf delta =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { pos with
    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
    Lexing.pos_bol = pos.Lexing.pos_cnum - delta;
  }
;;
}

let newline    = '\n' | "\r\n"
let whitespace = [' ' '\t']
let digit      = ['0'-'9']
let integer    = digit+
let alpha      = ['_' 'a'-'z' 'A'-'Z']
let alphanum   = alpha | digit
let identifier = alpha alphanum*
let backslash_escapes = ['\\' '"' '\'' 'n' 't' 'b' 'r']
let invalidcstr_char  = ['{' '}' ';' '#' '"' ''' ] | "/*" | "*/" | "//"
let cstr_cast = ['('] whitespace* alpha alphanum* whitespace* [')']
let cstr_libcall = alpha alphanum* ['(']
let consecutive_strings = ['"'] whitespace* ['"']

rule token = parse
    newline            { Lexing.new_line lexbuf; token lexbuf }
  | whitespace         { token lexbuf }
  | "/*"               { comment 0 lexbuf }

  (* string parsing from OCaml compiler code :-) *)
  | '"'
    { reset_string_buffer ();
      parse_string lexbuf;
      (*handle_lexical_error string lexbuf;*)
      LITSTR(get_stored_string ()) }

  | "#["
    { reset_string_buffer ();
      reset_idlist ();
      parse_cstr lexbuf;
      CSTR(get_stored_string (), !idlist) }

  (* operators *)
  | "**"               { CONVOP   }
  | ":="               { DEFINE   }
  | "|="               { OREQUAL  }
  | '='                { ASSIGN   }
  | '|'                { PIPE     }
  | '@'                { ATSYM    }
  | '-'                { UMINUS   }
  | '/'                { FSLASH   }
  | '+'                { UPLUS    }

  (* punctuation *)
  | '('                { LPAREN   }
  | ')'                { RPAREN   }
  | '<'                { LTCHAR   }
  | '>'                { GTCHAR   }
  | '['                { LBRKT    }
  | ']'                { RBRKT    }
  | '{'                { LBRACE   }
  | '}'                { RBRACE   }
  | ';'                { SEMI     }
  | ':'                { COLON    }
  | ','                { COMMA    }

  (* built-in types *)
  | "Image"            { IMAGET   }
  | "Kernel"           { KERNELT  }
  | "Calc"             { CALCT    }
  | "Uint8"            { UINT8T   }
  | "Uint16"           { UINT16T  }
  | "Uint32"           { UINT32T  }
  | "Int8"             { INT8T    }
  | "Int16"            { INT16T   }
  | "Int32"            { INT32T   }
  | "Angle"            { ANGLET   }

  (* library functions *)
  | "imgread"          { IMGREAD  }
  | "imgwrite"         { IMGWRITE }

  | integer as lit     { INTEGER(int_of_string lit) }
  | identifier as lit  { ID(lit) }
  | eof                { EOF }
  | _ as char          { raise (LexError("illegal character '" ^
                                        (Char.escaped char^"'"))) }

(* fancy string parsing from OCaml compiler code :-) *)
and parse_string = parse
    consecutive_strings  { parse_string lexbuf }
  | '"'     { () }
  | newline { Lexing.new_line lexbuf; parse_string lexbuf }
  | '\\' ("\010" | "\013" | "\013\010") ([' ' '\009'] * as spaces)
    { incr_loc lexbuf (String.length spaces);
      parse_string lexbuf }
  | '\\' (backslash_escapes as c)
    { store_string_char(char_for_backslash c);
      parse_string lexbuf }
  | '\\' 'x' (['0'-'9' 'a'-'f' 'A'-'F'] as d) (['0'-'9' 'a'-'f' 'A'-'F'] as u)
    { store_string_char (char_for_hexadecimal_code d u) ;
      parse_string lexbuf }
  | '\\' (['0'-'9'] as c) (['0'-'9'] as d) (['0'-'9']  as u)
    { let v = decimal_code c d u in
      if v > 255 then
       lex_warning lexbuf
        (Printf.sprintf
          "illegal backslash escape in string: `\\%c%c%c'" c d u) ;
      store_string_char (Char.chr v);
      parse_string lexbuf }
  | '\\' (_ as c)
    { lex_warning lexbuf
        (Printf.sprintf "illegal backslash escape in string: `\\%c'" c) ;
      store_string_char '\\' ;
      store_string_char c ;
      parse_string lexbuf }
  | '\010'
    { store_string_char '\010';
      incr_loc lexbuf 0;
      parse_string lexbuf }
  | eof { raise(LexError("unterminated string")) }
  | _ as c
    { store_string_char c;
      parse_string lexbuf }

(* handle escaped-C sequences: do some basic sanity parsing
 * (to ensure that nothing crazy is going on) *)
and parse_cstr = parse
    "]#"    { () }
  | newline            { Lexing.new_line lexbuf; parse_cstr lexbuf }
  | whitespace         { parse_cstr lexbuf }
  | ")"
        { raise (LexError("Unmatched ')' in escaped C")) }
  | invalidcstr_char
        { raise (LexError("Invalid character in escaped-C string")) }
  | cstr_cast as cast
        { store_string_snip cast; parse_cstr lexbuf }
  | cstr_libcall | "(" as str
        { store_string_snip str; parse_cstr_libcall 0 lexbuf }
  | identifier as id
        { store_string_snip id; add_id_to_list id; parse_cstr lexbuf }
  | eof
        { raise (LexError("unterminated escaped-C string!")) }
  | _ as c
        { store_string_char c;
          parse_cstr lexbuf }

and parse_cstr_libcall level = parse
    "]#" { raise (LexError("Mismatched parens in escaped-C string")) }
  | newline            { Lexing.new_line lexbuf; parse_cstr_libcall level lexbuf }
  | whitespace         { parse_cstr_libcall level lexbuf }
  | ")" { store_string_char ')';
          if level = 0 then
            parse_cstr lexbuf
          else
            parse_cstr_libcall (level-1) lexbuf }
  | invalidcstr_char
        { raise (LexError("Invalid character in escaped-C string")) }
  | cstr_cast as cast
        { store_string_snip cast; parse_cstr_libcall (level) lexbuf }
  | cstr_libcall | "(" as str
        { store_string_snip str;
          parse_cstr_libcall (level+1) lexbuf }
  | identifier as id
        { store_string_snip id; add_id_to_list id; parse_cstr_libcall level lexbuf }
  | eof { raise (LexError("unterminated function call in escaped-C string")) }
  | _ as c
    { store_string_char c;
      parse_cstr_libcall level lexbuf }

and comment level = parse
    "*/"  { if level = 0 then token lexbuf
            else comment (level-1) lexbuf }
  | newline { Lexing.new_line lexbuf; comment level lexbuf }
  | "/*"    { comment (level+1) lexbuf }
  | eof     { raise (LexError("unterminated comment!")) }
  | _       { comment level lexbuf }

and tokTail acc = parse
    eof { acc }
  | _* as str { tokTail (acc ^ str) lexbuf }

