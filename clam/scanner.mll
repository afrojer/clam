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

rule token = parse
    [' ' '\t']         { token lexbuf }
  | ['\r' '\n']        { Lexing.new_line lexbuf; token lexbuf }
  | "/*"               { comment lexbuf }
  | ';'                { SEMI     }
  | ':'                { COLON    }
  | ','                { COMMA    }
  | "$("               { DLPAREN  }
  | '('                { LPAREN   }
  | ')'                { RPAREN   }
  | '<'                { LTCHAR   }
  | '>'                { GTCHAR   }
  | "**"               { CONVOP   }
  | '|'                { PIPE     }
  | '@'                { ATSYM    }
  | '-'                { UMINUS   }
  | '='                { ASSIGN   }
  | ":="               { DEFINE   }
  | "|="               { OREQUAL  }
(*
  | '/'                { FSLASH   }
  | '+'                { PLUS     }
*)
  (* built-in types *)
  | "Image"            { IMAGET   }
  | "Kernel"           { KERNELT  }
  | "Calc"             { CALCT    }
  | "Channel"          { CHANNELT }
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

  (* literals / identifiers *)
  | '"'_*'"' as litstr { LITSTR(litstr) }
  | "#["_*"]#" as cstr { CSTR(String.sub cstr 2 ((String.length cstr) - 4)) }
  | ['0'-'9']+ as lit  { INTEGER(int_of_string lit) }
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']* as lit { ID(lit) }
  | eof                { EOF }
  | _ as char          { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/"  { token lexbuf }
  | _   { comment lexbuf }

and tokTail acc = parse
  | eof { acc }
  | _* as str { tokTail (acc ^ str) lexbuf }

