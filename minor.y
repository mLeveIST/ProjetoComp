%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int yylex();
void yyerror(char *s);
%}

%token PROG MODL STRT END

%%

S :
  ;

%%
