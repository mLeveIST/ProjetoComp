%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "node.h"

#define YYDEBUG 1

int yylex(), yyerror(char *s), yyparse();
%}

%union {
  int i;
  char *s;
  Node *n;
}

%token PROGRAM MODULE START END
%token VOID CONS NUMTYPE ARRTYPE STRTYPE
%token FUNC PUBL FRWD RETURN
%token IF THEN ELIF ELSE FI
%token FOR UNTIL STEP DO REPEAT STOP DONE

%token <i> NUM CHA
%token <s> ID STR 

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

%type <n> prog modl opdecls decls decl
%type <n> func opvars vars var qualf cons
%type <n> type lits litexp lit body bvars
%type <n> insts inst elifs block opretn retn
%type <n> expr leftv args

%token NIL ERR PROG MODL DECLS DECL VARS VAR LIT_EXP QUALIFIER TYPE LEFT_VAL ARGS CALL BODY BODY_VARS ARR_CHAIN INSTS BLOCK RETN STP REP ALLOC

%%

file : prog                   { printNode($1, 0, (char**) yyname); freeNode($1); }
     | modl                   { printNode($1, 0, (char**) yyname); freeNode($1); }
     ;

prog : PROGRAM opdecls START body END               { $$ = binNode(PROG, $2, $4); }
     ;

modl : MODULE opdecls END     { $$ = uniNode(MODL, $2); }
     ;

opdecls :                     { $$ = nilNode(NIL); }
        | decls               { $$ = $1 }
        ;

decls : decl                  { $$ = binNode(DECLS, nilNode(NIL), $1); }
      | decls ';' decl        { $$ = binNode(DECLS, $1, $3); }
      ;

decl : func                                         { $$ = $1; }
     | qualf cons var                               { $$ = quadNode(DECL, $1, $2, $3, nilNode(NIL)); }
     | qualf cons var ASSOC lits                    { $$ = quadNode(DECL, $1, $2, $3, $5); }
     ;

func : FUNC qualf VOID ID opvars DONE               {}
     | FUNC qualf type ID opvars DONE               {}
     | FUNC qualf VOID ID opvars DO body            {}
     | FUNC qualf type ID opvars DO body opretn     {}
     ;

opvars :                      { $$ = nilNode(NIL); }
       | vars                 { $$ = $1 }
       ;

vars : var                    { $$ = binNode(VARS, nilNode(NIL), $1); }
     | vars ';' var           { $$ = binNode(VARS, $1, $3); }
     ;

var : type ID                 { $$ = triNode(VAR, $1, strNode(ID, $2), nilNode(NIL)); }
    | type ID '[' NUM ']'     { $$ = triNode(VAR, $1, strNode(ID, $2), intNode(NUM, $4)); }
    ;

qualf :                       { $$ = nilNode(NIL); }
      | PUBL                  { $$ = nilNode(QUALIFIER); }
      | FRWD                  { $$ = nilNode(QUALIFIER); }
      ;

cons :                        { $$ = nilNode(NIL); }
     | CONS                   { $$ = nilNode(CONS); }
     ;

type : NUMTYPE                { $$ = nilNode(TYPE); }
     | STRTYPE                { $$ = nilNode(TYPE); }
     | ARRTYPE                { $$ = nilNode(TYPE); }
     ;

lits : litexp                 { $$ = $1; }
     | lits ',' NUM           { $$ = binNode(ARR_CHAIN, $1, $3); }
     ;

litexp : lit                  { $$ = binNode(LIT_EXP, nilNode(NIL), $1); }
       | litexp lit           { $$ = binNode(LIT_EXP, $1, $2); }
       ;

lit : NUM                     { $$ = intNode(NUM, $1); }
    | CHA                     { $$ = intNode(CHA, $1); }
    | STR                     { $$ = strNode(STR, $1); }
    ;

body : bvars insts            { $$ = binNode(BODY, $1, $2); }
     ;

bvars :                       { $$ = nilNode(NIL); }
      | bvars var ';'         { $$ = binNode(BODY_VARS, $1, $2); }
      ;

insts :                       { $$ = nilNode(NIL); }
      | insts inst            { $$ = binNode(INSTS, $1, $2); }
      ;

inst : IF expr THEN block elifs FI                     { $$ = triNode(IF, $2, $4, $5); }
     | IF expr THEN block elifs ELSE block FI          { $$ = binNode(ELSE, triNode(IF, $2, $4, $5), $7); }
     | FOR expr UNTIL expr STEP expr DO block DONE     { $$ = quadNode(FOR, $2, $4, $6, $8); }
     | expr ';'                                        { $$ = $1; }
     | expr '!'                                        { $$ = uniNode('!', $1); }
     | REPEAT                                          { $$ = nilNode(REP); }
     | STOP                                            { $$ = nilNode(STP); }
     | leftv '#' expr ';'                              { $$ = binNode(ALLOC, $3, $1); }
     ;

elifs :                                   { $$ = nilNode(NIL); }
      | elifs ELIF expr THEN block        { $$ = triNode(ELIF, $1, $3, $5); }
      ;

block : insts opretn                      { $$ = binNode(BLOCK, $1, $2); }
      ;

opretn :                                  { $$ = nilNode(NIL); }
       | retn                             { $$ = $1; }

retn : RETURN                             { $$ = uniNode(RETN, nilNode(NIL)); }
     | RETURN expr                        { $$ = uniNode(RETN, $2); }

expr : leftv                              { $$ = $1; }
     | litexp                             { $$ = $1; }
     | '(' expr ')'                       { $$ = $2; }
     | expr '(' args ')'                  { $$ = binNode(CALL, $1, $3); }
     | expr '(' args ')' '[' expr ']'     { $$ = binNode(CALL, $1, $3); }
     | '?'                                { $$ = nilNode('?'); }
     | '&' leftv %prec ADDR               { $$ = uniNode(ADDR, $2); }
     | '-' expr %prec UMINUS              { $$ = uniNode(UMINUS, $2); }
     | expr '^' expr                      { $$ = binNode('^', $1, $3); }
     | expr '*' expr                      { $$ = binNode('*', $1, $3); }
     | expr '/' expr                      { $$ = binNode('/', $1, $3); }
     | expr '%' expr                      { $$ = binNode('%', $1, $3); }
     | expr '+' expr                      { $$ = binNode('+', $1, $3); }
     | expr '-' expr                      { $$ = binNode('-', $1, $3); }
     | expr '<' expr                      { $$ = binNode('<', $1, $3); }
     | expr '>' expr                      { $$ = binNode('>', $1, $3); }
     | expr LE expr                       { $$ = binNode(LE, $1, $3); }
     | expr GE expr                       { $$ = binNode(GE, $1, $3); }
     | expr '=' expr                      { $$ = binNode('=', $1, $3); }
     | expr NE expr                       { $$ = binNode(NE, $1, $3); }
     | '~' expr                           { $$ = uniNode('~', $2); }
     | expr '&' expr                      { $$ = binNode('&', $1, $3); }
     | expr '|' expr                      { $$ = binNode('|', $1, $3); }
     | leftv ASSOC expr                   { $$ = binNode(ASSOC, $1, $3); }
     ;

leftv : ID                  { $$ = binNode(LEFT_VAL, strNode(ID, $1), nilNode(NIL)); }
      | ID '[' expr ']'     { $$ = binNode(LEFT_VAL, strNode(ID, $1), $3); }
      ;

args : expr                 { $$ = binNode(ARGS, nilNode(NIL), $1); }
     | args ',' expr        { $$ = binNode(ARGS, $1, $3); }
     ;

%%

char **yynames =
#if YYDEBUG > 0
  (char**)yyname;
#else
  0;
#endif
