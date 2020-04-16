%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "node.h"

#define YYDEBUG 1

int yylex(), yyerror(char *s), yyparse();

Node *funcNode(Node *n1, Node *n2, Node *n3, Node *n4, Node *n5, char *id);
%}

%union {
  int i;
  char *s;
  Node *n;
}

%token PROG MODL START END
%token VOID CONS NUMTYPE ARRTYPE STRTYPE
%token FUNC PUBL FRWD RETN
%token IF THEN ELIF ELSE FI
%token FOR UNTIL STEP DO REP STOP DONE

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
%type <n> func fbody fhead ftype params vars
%type <n> var qualf cons type lit body bvars
%type <n> insts inst elifs block opretn retn
%type <n> expr leftv args lits numlst litlst

%token NIL ERR DECLS DECL VARS VAR LIT_EXP LEFT_VAL ARGS CALL
%token BODY BODY_VARS ARRAY INSTS BLOCK ALLOC IFELSE CONDT RANGE
%token MODIFIERS VALUE INDEX FSIGNATURE VSIGNATURE FBODY FHEAD VALUE

%%

file : prog                   { printNode($1, 0, (char**) yyname); freeNode($1); }
     | modl                   { printNode($1, 0, (char**) yyname); freeNode($1); }
     ;

prog : PROG opdecls START body END     { $$ = binNode(PROG, $2, $4); }
     ;

modl : MODL opdecls END       { $$ = uniNode(MODL, $2); }
     ;

opdecls :                     { $$ = nilNode(NIL); }
        | decls               { $$ = $1; }
        ;

decls : decl                  { $$ = binNode(DECLS, nilNode(NIL), $1); }
      | error                 { $$ = binNode(DECLS, nilNode(NIL), nilNode(ERR)); }
      | decls ';' decl        { $$ = binNode(DECLS, $1, $3); }
      | decls ';' error       { $$ = binNode(DECLS, $1, nilNode(ERR)); }
      ;

decl : func                          { $$ = $1; }
     | qualf cons var                { $$ = binNode(DECL, binNode(MODIFIERS, $1, $2), binNode(VSIGNATURE, $3, nilNode(NIL))); }
     | qualf cons var ASSOC lits     { $$ = binNode(DECL, binNode(MODIFIERS, $1, $2), binNode(VSIGNATURE, $3, $5)); }
     ;

func : FUNC fhead fbody       { $$ = binNode(FUNC, $2, $3); }
     ;

fbody : DONE                  { $$ = nilNode(DONE); }
      | DO body opretn        { $$ = binNode(FBODY, $2, $3); } 
      ;

fhead : qualf ftype params    { $$ = binNode(FHEAD, $1, binNode(FSIGNATURE, $2, $3)); }
      ;

ftype : VOID ID               { $$ = binNode(VAR, nilNode(VOID), strNode(ID, $2)); }
      | type ID               { $$ = binNode(VAR, $1, strNode(ID, $2)); }
      ;

params :                      { $$ = nilNode(NIL); }
       | vars                 { $$ = $1; }
       ;

vars : var                    { $$ = binNode(VARS, nilNode(NIL), $1); }
     | vars ';' var           { $$ = binNode(VARS, $1, $3); }
     ;

var : type ID                 { $$ = binNode(VAR, $1, strNode(ID, $2)); }
    | type ID '[' NUM ']'     { $$ = binNode(INDEX, binNode(VAR, $1, strNode(ID, $2)), intNode(NUM, $4)); }
    ;

qualf :                       { $$ = nilNode(NIL);
                                $$->info = 0; }
      | PUBL                  { $$ = nilNode(PUBL);
                                $$->info = PUBL; }
      | FRWD                  { $$ = nilNode(FRWD);
                                $$->info = FRWD; }
      ;

cons :                        { $$ = nilNode(NIL);
                                $$->info = 0; }
     | CONS                   { $$ = nilNode(CONS);
                                $$->info = CONS; }
     ;

type : NUMTYPE                { $$ = nilNode(NUMTYPE);
                                $$->info = NUMTYPE; }
     | STRTYPE                { $$ = nilNode(STRTYPE);
                                $$->info = STRTYPE; }
     | ARRTYPE                { $$ = nilNode(ARRTYPE);
                                $$->info = ARRTYPE; }
     ;

lits : numlst                 { $$ = $1; }
     | litlst                 { $$ = $1; }
     ;

numlst : NUM ',' NUM          { $$ = binNode(ARRAY, intNode(NUM, $1), intNode(NUM, $3)); }
       | numlst ',' NUM       { $$ = binNode(ARRAY, $1, intNode(NUM, $3)); }
       ;

litlst : lit                  { $$ = binNode(VALUE, nilNode(NIL), $1); }
       | litlst lit           { $$ = binNode(VALUE, $1, $2); }
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
      | insts error           { $$ = binNode(INSTS, $1, nilNode(ERR)); }
      ;

inst : IF expr THEN block elifs FI                     { $$ = binNode(IF, binNode(CONDT, $2, $4), $5); }
     | IF expr THEN block elifs ELSE block FI          { $$ = binNode(IFELSE, binNode(IF, binNode(CONDT, $2, $4), $5), $7); }
     | FOR expr UNTIL expr STEP expr DO block DONE     { $$ = binNode(FOR, binNode(RANGE, $2, binNode(DO, $4, $6)), $8); }
     | expr ';'                                        { $$ = $1; }
     | expr '!'                                        { $$ = uniNode('!', $1); }
     | REP                                             { $$ = nilNode(REP); }
     | STOP                                            { $$ = nilNode(STOP); }
     | leftv '#' expr ';'                              { $$ = binNode(ALLOC, $3, $1); }
     ;

elifs :                                   { $$ = nilNode(NIL); }
      | elifs ELIF expr THEN block        { $$ = binNode(ELIF, $1, binNode(CONDT, $3, $5)); }
      ;

block : insts opretn                      { $$ = binNode(BLOCK, $1, $2); }
      ;

opretn :                                  { $$ = nilNode(NIL); }
       | retn                             { $$ = $1; }

retn : RETN                               { $$ = uniNode(RETN, nilNode(NIL)); }
     | RETN expr                          { $$ = uniNode(RETN, $2); }

expr : leftv                              { $$ = $1; }
     | litlst                             { $$ = $1; }
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


