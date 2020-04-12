%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define YYDEBUG 1

int yylex(), yyerror(char *s), yyparse();
%}

%token PROG MODL STRT END

%%

S :
  ;

%%
