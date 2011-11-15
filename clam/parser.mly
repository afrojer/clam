%{(*
   * File: parser.mly
   * Date: 2011-10-10
   *
   * PLT Fall 2011
   * CLAM Project
   * Jeremy C. Andrus <jeremya@cs.columbia.edu>
   * Robert Martin <rdm2128@columbia.edu>
   * Kevin Sun <kfs2110@columbia.edu>
   * Yongxu Zhang <yz2419@columbia.edu>
   *
   *)

open Clamtypes
open Ast %}

%token SEMI LPAREN DLPAREN RPAREN LTCHAR GTCHAR LBRKT RBRKT LBRACE RBRACE
%token COLON COMMA FSLASH
%token CONVOP PIPE ATSYM UMINUS UPLUS
%token ASSIGN DEFINE OREQUAL
%token IMAGET KERNELT CALCT CHANNELT
%token UINT8T UINT16T UINT32T INT8T INT16T INT32T ANGLET
%token IMGREAD IMGWRITE
%token <string> LITSTR
%token <string> CSTR
%token <int> INTEGER
%token <string> ID
%token EOF

%right ASSIGN DEFINE OREQUAL
%left PIPE COMMA
%left CONVOP
%left COLON
%right UMINUS UPLUS ATSYM

%start program
%type <Ast.program> program

%%

program:
  /* nothing */  { [] }
  | program stmt { $2 :: $1 }

atom:
    UINT8T  { Uint8  }
  | UINT16T { Uint16 }
  | UINT32T { Uint32 }
  | INT8T   { Int8   }
  | INT16T  { Int16  }
  | INT32T  { Int32  }
  | ANGLET  { Angle  }

libfunc:
    IMGREAD     { ImgRead }
  | IMGWRITE    { ImgWrite }

chanref:
    ID COLON ID { { image = $1; channel = $3; } }

bareint:
    INTEGER                      { BInt($1)  }
  | UMINUS INTEGER               { BInt(-$2) }
  | UPLUS INTEGER                { if $2 < 0 then
                                     BInt(0-$2) else
                                     BInt($2) }

/* tuple:
 *   fst = list of IDs (channels) to calculate
 *   snd = list of IDs (channels) whose output is discarded
 */
kerncalc:
    ID PIPE ID                  { ($3 :: [$1]), [] }
  | ID PIPE ATSYM ID            { ($4 :: [$1]), [$4] }
  | ATSYM ID PIPE ID            { ($4 :: [$2]), [$2] }
  | ATSYM ID PIPE ATSYM ID      { ($5 :: [$2]), ($5 :: [$2]) }
  | kerncalc PIPE ID            { ($3 :: fst $1), snd $1 }
  | kerncalc PIPE ATSYM ID      { ($4 :: fst $1), ($4 :: snd $1) }

matrix_scale:
    LBRKT bareint FSLASH bareint RBRKT { ($2, $4) }

matrix_row:
    bareint { [$1] }
  | matrix_row bareint { $2 :: $1 }

matrix:
    LBRACE matrix_row RBRACE    { [List.rev $2]       }
  | LBRACE matrix_row COMMA     { [List.rev $2]       }
  | matrix matrix_row COMMA     { (List.rev $2) :: $1 }
  | matrix matrix_row RBRACE    { (List.rev $2) :: $1 }

vdecl:
    IMAGET ID                   { ImageT($2) }
  | KERNELT ID                  { KernelT($2) }
  | CALCT ID                    { CalcT($2, Uint8) }
  | CALCT ID LTCHAR atom GTCHAR { CalcT($2, $4) }

expr:
    ID                           { Id($1) }
  | bareint                      { Integer($1) }
  | LITSTR                       { LitStr($1) }
  | CSTR                         { CStr($1) }
  | kerncalc                     { KernCalc($1) }
  | DLPAREN chanref RPAREN       { ChanEval($2) }
  | matrix_scale matrix          { ChanMat($1, List.rev $2) }
  | chanref                      { ChanRef($1) }
  | expr CONVOP expr             { Convolve($1, $3) }
  | ID ASSIGN expr               { Assign($1, Eq, $3) }
  | ID OREQUAL expr              { Assign($1, OrEq, $3) }
  | chanref ASSIGN expr          { ChanAssign($1, $3) }
  | libfunc LPAREN libfunc_args RPAREN { LibCall($1, $3) }

libfunc_args:
    expr                         { [$1] }
  | libfunc_args COMMA expr      { $3 :: $1 }

stmt:
    expr SEMI                    { Expr($1)     }
  | vdecl SEMI                   { VDecl($1)    }
  | vdecl DEFINE expr SEMI       { VAssign($1, DefEq, $3) }
  | vdecl ASSIGN expr SEMI       { VAssign($1, Eq, $3) }

