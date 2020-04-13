%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define YYDEBUG 1

int yylex(), yyerror(char *s), yyparse();
%}

%union {
  int i;
  char *s;
}

%token PROG MODL START END
%token VOID CONS IDNUM IDVEC IDSTR
%token FUNC PUBL FRWD RETN
%token IF THEN ELIF ELSE FI
%token FOR UNTIL STEP DO REP STOP DONE

%token <i> NUM CHA
%token <s> ID STR

%nonassoc ELSE
%right IF ELIF
%right ASSOC
%left '|'
%left '&'
%nonassoc '~'
%left '=' NE
%left '<' '>' LE GE
%left '+' '-'
%left '*' '/' '%'
%right '^'
%nonassoc '?' UMINUS ADDR
%nonassoc '(' '['

%%

file : prog 
     | modl
     ;

prog : PROG opdecls START body END
     ;

modl : MODL opdecls END
     ;

opdecls :
        | decls
        ;

decls : decl
      | decls ';' decl
      ;

decl : func
     | qualf cons var
     | qualf cons var ASSOC lits
     ;

func : FUNC qualf types ID opvars DONE
     | FUNC qualf types ID opvars DO body
     ;

opvars :
       | vars
       ;

vars : var
     | vars ';' var
     ;

var : type ID
    | type ID '[' NUM ']'

qualf :
      | PUBL
      | FRWD
      ;

cons :
     | CONS
     ;

types : VOID
      | type
      ;

type : IDNUM
     | IDSTR
     | IDVEC
     ;

lits : lits ',' NUM
     | exprlit
     ;

exprlit : lit
        | exprlit lit
        ;

lit : NUM
    | CHA
    | STR
    ;

body : bvars insts
     ;

bvars :
      | bvars var ';'
      ;

insts :
      | inst
      ;

inst : IF expr THEN insts elifs FI
     | IF expr THEN insts elifs ELSE insts FI
     | FOR expr UNTIL expr STEP expr DO insts DONE
     | expr ';'
     | expr '!'
     | REP
     | STOP
     | RETN opexpr
     | leftv '#' expr ';'
     ;

elifs :
      | elifs ELIF expr THEN insts
      ;

opexpr :
       | expr
       ;

expr : leftv
     | exprlit
     | '(' expr ')'
     | ID '(' args ')'
     | ID '(' args ')' '[' expr ']'
     | '?'
     | '&' leftv %prec ADDR
     | '-' expr %prec UMINUS
     | expr '^' expr
     | expr '*' expr
     | expr '/' expr
     | expr '%' expr
     | expr '+' expr
     | expr '-' expr
     | expr '<' expr
     | expr '>' expr
     | expr LE expr
     | expr GE expr
     | expr '=' expr
     | expr NE expr
     | '~' expr
     | expr '&' expr
     | expr '|' expr
     | leftv ASSOC expr
     ;

leftv : ID
      | ID '[' expr ']'
      ;

args : expr
     | args ',' expr
     ;

%%

char **yynames =
#if YYDEBUG > 0
  (char**)yyname;
#else
  0;
#endif
