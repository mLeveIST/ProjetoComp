%{
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include "y.tab.h"

char *dupstr(const char *s);
extern void yyerror(const char *msg);

int decimalInt(char *s);
int octalInt(char *s);
int hexadecimalInt(char *s);
int binaryInt(char *s);
%}

%x IN_COM IN_STR

%%

<IN_COM>.|\n              ;
<IN_COM>\$                BEGIN 0;

\$\$.*                    ;               /* Line comment */
\$                        BEGIN IN_COM;   /* Block comment */

"program"                 return PROG;
"module"                  return MODL;
"start"                   return START;
"end"                     return END;
"void"                    return VOID;
"const"                   return CONS;
"number"                  return IDNUM;
"array"                   return IDVEC;
"string"                  return IDSTR;
"function"                return FUNC;
"public"                  return PUBL;
"forward"                 return FRWD;
"if"                      return IF;
"then"                    return THEN;
"elif"                    return ELIF;
"else"                    return ELSE;
"fi"                      return FI;
"for"                     return FOR;
"until"                   return UNTIL;
"step"                    return STEP;
"do"                      return DO;
"repeat"                  return REP;
"stop"                    return STOP;
"done"                    return DONE;
"return"                  return RETN;

[A-Za-z][A-Za-z0-9_]*     { yylval.s = dupstr(yytext); return ID; }

0|[1-9][0-9]*             { yylval.i = decimalInt(yytext); return NUM; }
0[0-7]+                   { yylval.i = octalInt(yytext + 1); return NUM; }
0x[0-9A-Fa-f]+            { yylval.i = hexadecimalInt(yytext + 2); return NUM; }
0b(0|1)+                  { yylval.i = binaryInt(yytext + 2); return NUM; }

[ \n\t\r]+                ;

.                         yyerror("ERROR : Unknown character!");

%%
int yywrap()
{
  if (YYSTATE == IN_COM)
    yyerror("ERROR : Unterminated comment!");
  if (YYSTATE == IN_STR)
    yyerror("ERROR : Unterminated string!");
  return 1;
}
char *getyytext() { return yytext; }

int decimalInt(char *s)
{
  errno = 0;
  int num = strtol(s, 0, 0);

  if (errno == ERANGE)
    yyerror("ERROR : Overflow in decimal number!");

  return num;
}

int octalInt(char *s)
{
  int i, num = 0, aux = 0;

  for (i = 0; i < strlen(s); i++) {
    if (s[i] >= '0' && s[i] <= '7')
      aux = aux * 8 + s[i] - '0';
    else
      /* ERROR in use of function */

    if (aux < num) {
      yyerror("ERROR : Overflow in octal number!");
      break;
    }
    num = aux;
  }
  return num;
}

int hexadecimalInt(char *s)
{
  int i, num = 0, aux = 0;

  for (i = 0; i < strlen(s); i++) {
    if (s[i] >= '0' && s[i] <= '9')
      aux = aux * 16 + s[i] - '0';
    else if (s[i] >= 'A' && s[i] <= 'F')
      aux = aux * 16 + 10 + s[i] - 'A';
    else if (s[i] >= 'a' && s[i] <= 'f')
      aux = aux * 16 + 10 + s[i] - 'a';
    else
      /* ERROR in use of function */

    if (aux < num) {
      yyerror("ERROR : Overflow in hexadecimal number!");
      break;
    }
    num = aux;
  }
  return num;
}

int binaryInt(char *s)
{
  int i, num = 0, aux = 0;

  for (i = 0; i < strlen(s); i++) {
    if (s[i] == '0' || s[i] == '1')
      aux = aux * 2 + s[i] - '0';
    else
      /* ERROR in use of function */

    if (aux < num) {
      yyerror("ERROR : Overflow in binary number!");
      break;
    }
    num = aux;
  }
  return num;
}
