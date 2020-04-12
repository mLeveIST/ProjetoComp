%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define YYDEBUG 1

int yylex(), yyerror(char *s), yyparse();
%}

%union {
  int i;
  int *v;
  char *s;
}

%token PROG MODL START END
%token VOID CONST IDNUM IDVEC IDSTR
%token FUNC PUBL FRWD RETN
%token IF THEN ELIF ELSE FI
%token FOR UNTIL STEP DO REP STOP DONE

%token <i> NUM
%token <v> VEC
%token <s> ID STR

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
     | qualf cons var ASSOC oplits
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

oplits :
       | lits1
       | lits2
       ;

lits1 : lit
      | lits1 ',' lit
      ;

lits2 : lit
      | lits2 ',' lit
      ;

lit : NUM
    | STR
    | VEC
    ;

body : bvars inst
     ;

bvars :
      | bvars var ';'
      ;

inst : 
     | IF expr THEN inst elifs FI
     | IF expr THEN inst elifs ELSE inst FI
     | FOR expr UNTIL expr STEP expr DO inst DONE
     | expr ';'
     | expr '!'
     | REP
     | STOP
     | RETN
     | RETN expr
     | leftv '#' expr ';'
     ;

elifs :
      | elifs ELIF expr THEN inst
      ;

expr : 'e'

leftv : 'l'

%%
char **yynames =
#if YYDEBUG > 0
  (char**)yyname;
#else
  0;
#endif
