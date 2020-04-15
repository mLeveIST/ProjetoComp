%{
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "node.h"
#include "y.tab.h"

char *dupstr(const char *s);
extern void yyerror(const char *msg);

char *convert_hexa(char *s);
int parse_char(char *s);
int parse_decimal(char *s);
int parse_octal(char *s);
int parse_hexa(char *s);
int parse_binary(char *s);
%}

%option debug

%x IN_CODE IN_COM IN_STR

HEXDIG [0-9A-Fa-f]
HEXCHA \\{HEXDIG}{HEXDIG}?
WSP [ \n\t\r]

%%

^program                   { BEGIN IN_CODE; return PROGRAM; }
^module                    { BEGIN IN_CODE; return MODULE; }

.|\n                       ;

<IN_COM>\$                 BEGIN IN_CODE;
<IN_COM>.|\n               ;

<IN_CODE>{

\$\$.*                     ;               /* Line comment */
\$                         BEGIN IN_COM;   /* Block comment */

^end                       { BEGIN INITIAL; return END; }
"start"                    return START;
"void"                     return VOID;
"const"                    return CONS;
"number"                   return NUMTYPE;
"array"                    return ARRTYPE;
"string"                   return STRTYPE;
"function"                 return FUNC;
"public"                   return PUBL;
"forward"                  return FRWD;
"if"                       return IF;
"then"                     return THEN;
"elif"                     return ELIF;
"else"                     return ELSE;
"fi"                       return FI;
"for"                      return FOR;
"until"                    return UNTIL;
"step"                     return STEP;
"do"                       return DO;
"repeat"                   return REPEAT;
"stop"                     return STOP;
"done"                     return DONE;
"return"                   return RETURN;

":="                       return ASSOC;
"<="                       return LE;
">="                       return GE;
"~="                       return NE;

[A-Za-z][A-Za-z0-9_]*      { yylval.s = dupstr(yytext); return ID; }

\'([^'\\]|\\[ntr'\\])?\'   { yytext[yyleng-1] = '\0'; yylval.i = parse_char(yytext + 1); return CHA; }
  /* \'\\0\'                    { yylval.i = '\0'; return CHA; } */
\'{HEXCHA}\'               { yytext[yyleng-1] = '\0'; yylval.i = parse_hexa(yytext + 2); return CHA; }

\"([^"\\]|\\[ntr0"\\]|{HEXCHA})*\"  { yylval.s = malloc(yyleng); *yylval.s = 0; REJECT; }
\"                         BEGIN IN_STR;

0|[1-9][0-9]*              { yylval.i = parse_decimal(yytext); return NUM; }
0[0-7]+                    { yylval.i = parse_octal(yytext + 1); return NUM; }
0x{HEXDIG}+                { yylval.i = parse_hexa(yytext + 2); return NUM; }
0b(0|1)+                   { yylval.i = parse_binary(yytext + 2); return NUM; }

[-+/%*^<>=|&~?#\[\]();!,]  return *yytext;

{WSP}+                     ;
.                          yyerror("ERROR : Unknown character!");

}

<IN_STR>{

\"                         { BEGIN IN_CODE; return STR; }
\\n                        strcat(yylval.s, "\n");
\\t                        strcat(yylval.s, "\t");
\\r                        strcat(yylval.s, "\r");
  /* \\0/[^0-9A-Fa-f]           strcat(yylval.s, "\0"); */
\\[\\"]                    strcat(yylval.s, yytext + 1);
{HEXCHA}                   strcat(yylval.s, convert_hexa(yytext + 1));
\\*                        yyerror("ERROR : Invalid escape sequence in string literal!");
.|\n                       strcat(yylval.s, yytext);

}

%%
int yywrap()
{
  if (YYSTATE == IN_CODE)
    yyerror("ERROR : Unterminated code region!");
  if (YYSTATE == IN_COM)
    yyerror("ERROR : Unterminated comment!");
  if (YYSTATE == IN_STR)
    yyerror("ERROR : Unterminated string!");
  return 1;
}
char *getyytext() { return yytext; }

char *convert_hexa(char *s)
{
  s[0] = parse_hexa(s);
  s[1] = '\0';
  return s;
}

int parse_char(char *s)
{
  if (strlen(s) == 0)
    return 0;

  if (strlen(s) == 1 && s[0] != '\\' && s[0] != '\'') {
    return s[0];
  } else if (strlen(s) == 2 && s[0] == '\\') {
    switch (s[1]) {
      case 'n': return '\n';
      case 't': return '\t';
      case 'r': return '\r';
      case '0': return '\0';
      case '\'': return '\'';
      case '\\': return '\\';
      default: yyerror("ERROR : Invalid character escape sequence!"); break;
    }
  } else
    /* ERROR in use of function */
    yyerror("ERROR : Invalid character sequence!");

  return 0;
}

int parse_decimal(char *s)
{
  errno = 0;
  int num = strtol(s, 0, 0);

  if (errno == ERANGE)
    yyerror("ERROR : Overflow in decimal number!");

  return num;
}

int parse_octal(char *s)
{
  int i, num = 0, aux = 0;

  for (i = 0; i < strlen(s); i++) {
    if (s[i] >= '0' && s[i] <= '7')
      aux = aux * 8 + s[i] - '0';
    else
      /* ERROR in use of function */
      yyerror("ERROR : Invalid octal number!");

    if (aux < num) {
      yyerror("ERROR : Overflow in octal number!");
      break;
    }
    num = aux;
  }
  return num;
}

int parse_hexa(char *s)
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
      yyerror("ERROR : Invalid hexadecimal number!");

    if (aux < num) {
      yyerror("ERROR : Overflow in hexadecimal number!");
      break;
    }
    num = aux;
  }
  return num;
}

int parse_binary(char *s)
{
  int i, num = 0, aux = 0;

  for (i = 0; i < strlen(s); i++) {
    if (s[i] == '0' || s[i] == '1')
      aux = aux * 2 + s[i] - '0';
    else
      /* ERROR in use of function */
      yyerror("ERROR : Invalid binary number!");

    if (aux < num) {
      yyerror("ERROR : Overflow in binary number!");
      break;
    }
    num = aux;
  }
  return num;
}
