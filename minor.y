%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define YYDEBUG 1

int yylex(), yyerror(char *s), yyparse();
%}

&union {
  int i;
  char *s;
}

%token PROG MODL START END
%token VOID CONST IDNUM IDVEC IDSTR
%token FUNC PUBL FRWD RETN
%token IF THEN ELIF ELSE FI
%token FOR UNTIL STEP DO REP STOP DONE

%token <i> NUM
%token <s> ID

%%

S :
  ;

%%
char **yynames =
#if YYDEBUG > 0
		 (char**)yyname;
#else
		 0;
#endif
