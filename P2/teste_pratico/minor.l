%{
#include <limits.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include "node.h"
#include "y.tab.h"

extern int yyerror(const char *);
static int strsz, intstr(char *s);
static char *str;

%}

%option yylineno
H [A-Fa-f0-9]
%x STRNG CODE FIM

%%

.|\n              ;

^"program"        BEGIN CODE; return PROGRAM;
^"module"         BEGIN CODE; return MODULE;

[a-zA-Z]+         ;

<CODE>{

"<="              return LE;
">="              return GE;
"~="              return NE;
":="              return ASSOC;
"$$".*            ;
\$[^\$]+\$        ;
"public"          return PUBLIC;
"forward"         return FORWARD;
"start"           return START;
^"end"            BEGIN FIM; return END;
"const"           return CONST;
"string"          return STRING;
"number"          return NUMBER;
"array"           return ARRAY;
"function"        return FUNCTION;
"void"            return VOID;
"if"              return IF;
"then"            return THEN;
"fi"              return FI;
"elif"            return ELIF;
"else"            return ELSE;
"for"             return FOR;
"until"           return UNTIL;
"step"            return STEP;
"do"              return DO;
"done"            return DONE;
"repeat"          return REPEAT;
"stop"            return STOP;
"return"          return RETURN;
[ \t\n\r]+        ;
"0x"{H}+          { yylval.i = intstr(yytext); return INT; }
"0b"[01]+         { yylval.i = intstr(yytext); return INT; }
0[0-7]+           { yylval.i = intstr(yytext); return INT; }
[0-9]+            { yylval.i = intstr(yytext); return INT; }
\"([^"\\]|\\.)*\" { strsz = yyleng; REJECT; }
\"                if (strsz > 0) { BEGIN STRNG; str = malloc(strsz); }
\'\\n\'           yylval.i = '\n'; return CHAR;
\'\\t\'           yylval.i = '\t'; return CHAR;
\'\\r\'           yylval.i = '\r'; return CHAR;
\'\\\'\'          yylval.i = '\''; return CHAR;
\'\\\\\'          yylval.i = '\\'; return CHAR;
\'\\{H}{H}?\'     yylval.i = strtol(yytext+2,0,16); return CHAR;
\'[^\\]\'         yylval.i = yytext[1]; return CHAR;

[a-zA-Z][a-zA-Z_0-9]*     { yylval.s = strdup(yytext); return ID; }

.                 return yytext[0];

}

<STRNG>{

\"          BEGIN CODE; yylval.s = str; return STR;
\\n         strcat(str, "\n");
\\t         strcat(str, "\t");
\\r         strcat(str, "\r");
\\\"        strcat(str, "\"");
\\\\        strcat(str, "\\");
\\{H}{H}?   { char s[2]; s[1] = 0; s[0] = strtol(yytext+1,0,16); strcat(str, s); }
.|\n        strcat(str, yytext);

}

<FIM>.|\n   return 0;

%%

int intstr(char *s)
{
  char *end;
  unsigned long ret;

  if (strlen(s) > 1 && s[1] == 'b')
    ret = strtoul(s+2, &end, 2);
  else
    ret = strtoul(s, &end, 0);
  if (*end != 0) return yyerror("conversion error in literal integer"), -1;
  if (ret > INT_MAX) return yyerror("overflow in literal integer"), -1;
  return ret;
}

int yywrap() { return 1; }
char *getyytext() { return yytext; }
