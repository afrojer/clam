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
open Ast %}

%token PLUS MINUS TIMES DIVIDE EOF
%token SEQ ASSIGN
%token <int> VARIABLE
%token <int> LITERAL

%left SEQ
%right ASSIGN
%left PLUS MINUS
%left TIMES DIVIDE

%start expr
%type <Ast.expr> expr

%%

expr:
    expr PLUS       expr { Binop($1, Add, $3) }
  | expr MINUS      expr { Binop($1, Sub, $3) }
  | expr TIMES      expr { Binop($1, Mul, $3) }
  | expr DIVIDE     expr { Binop($1, Div, $3) }
  | VARIABLE ASSIGN expr { Assign($1, $3) }
  | expr SEQ        expr { Seq($1, $3) }
  | VARIABLE             { Variable($1) }
  | LITERAL              { Lit($1) }
