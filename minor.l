%{
#include <stdlib.h>
#include <string.h>
#include "y.tab.h"

extern void yyerror(const char *msg);
%}

%%

"program"           return PROG;
"module"            return MODL;
"start"             return STRT;
"end"               return END;

[ \n\t\r]+          ;

.                   yyerror("ERROR : Unknown character!");

%%
int main() { return yylex(); }
int yywrap() { return 1; }
