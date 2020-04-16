%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "node.h"
#include "tabid.h"

#define YYDEBUG 1

int yylex(), yyerror(char *s), yyparse();

static void checkInFor();
static int checkAddress(Node *n1);
static int checkAssociation(Node *n1, Node *n2);
static int checkInvocation(char *id, Node *n2);
static int checkIndexation(char *id, Node *offset);
static int checkIfInt(Node *n1);
static int checkIntOp(Node *n1, Node *n2);
static int checkCompOp(Node *n1, Node *n2);

int in_for;
int qualifier, constant;
int is_null;
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
%type <n> insts inst elifs block endinst retn
%type <n> expr leftv args lits numlst litlst

%token NIL ERR DECLS DECL VARS VAR LIT_EXP LEFT_VAL ARGS CALL
%token BODY BODY_VARS ARRAY INSTS BLOCK ALLOC IFELSE CONDT RANGE
%token MODIFIERS VALUE INDEX FTYPE VTYPE FBODY FHEAD VALUE

%%

file : prog                   { /*printNode($1, 0, (char**) yyname); freeNode($1);*/ }
     | modl                   { /*printNode($1, 0, (char**) yyname); freeNode($1);*/ }
     ;

prog : PROG opdecls START { IDpush(); } body END     { $$ = binNode(PROG, $2, $4); IDpop(); }
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
     | qualf cons var                { $$ = binNode(DECL, binNode(MODIFIERS, $1, $2), binNode(VTYPE, $3, nilNode(NIL))); }
     | qualf cons var ASSOC lits     { $$ = binNode(DECL, binNode(MODIFIERS, $1, $2), binNode(VTYPE, $3, $5)); }
     ;

func : FUNC fhead fbody       { $$ = binNode(FUNC, $2, $3); IDpop(); }
     ;

fbody : DONE                  { $$ = nilNode(DONE); }
      | DO body endinst       { $$ = binNode(FBODY, $2, $3); } 
      ;

fhead : qualf ftype ID        { IDnew($2->info, $3, 0); /* add params */ }
        params                { $$ = binNode(FHEAD, $1, binNode(FTYPE, binNode(VAR, $2, strNode(ID, $3)), $5)); IDpush(); }
      ;

ftype : VOID                  { $$ = nilNode(VOID);
                                $$->info = VOID; }
      | type                  { $$ = $1;
                                $$->info = $1->info; }
      ;

params :                      { $$ = nilNode(NIL); }
       | vars                 { $$ = $1; }
       ;

vars : var                    { $$ = binNode(VARS, nilNode(NIL), $1); }
     | vars ';' var           { $$ = binNode(VARS, $1, $3); }
     ;

var : type ID                 { $$ = binNode(VAR, $1, strNode(ID, $2));
                                IDnew($1->info, $2, 0); }
    | type ID '[' NUM ']'     { $$ = binNode(INDEX, binNode(VAR, $1, strNode(ID, $2)), intNode(NUM, $4));
                                if ($1->info != ARRTYPE) yyerror("ERROR : Cannot define size for type array!");
                                IDnew(ARRTYPE, $2, 0); /* add size? */ }
    ;

qualf :                       { $$ = nilNode(NIL);
                                qualifier = 0; }
      | PUBL                  { $$ = nilNode(PUBL);
                                qualifier = PUBL; }
      | FRWD                  { $$ = nilNode(FRWD);
                                qualifier = FRWD; }
      ;

cons :                        { $$ = nilNode(NIL);
                                constant = 0; }
     | CONS                   { $$ = nilNode(CONS);
                                constant = CONS; }
     ;

type : NUMTYPE                { $$ = nilNode(NUMTYPE);
                                $$->info = NUMTYPE; }
     | STRTYPE                { $$ = nilNode(STRTYPE);
                                $$->info = STRTYPE; }
     | ARRTYPE                { $$ = nilNode(ARRTYPE);
                                $$->info = ARRTYPE; }
     ;

lits : numlst                 { $$ = $1;
                                $$->info = ARRTYPE; }
     | litlst                 { $$ = $1;
                                $$->info = $1->info; }
     ;

numlst : NUM ',' NUM          { $$ = binNode(ARRAY, intNode(NUM, $1), intNode(NUM, $3)); }
       | numlst ',' NUM       { $$ = binNode(ARRAY, $1, intNode(NUM, $3)); }
       ;

litlst : lit                  { $$ = binNode(VALUE, nilNode(NIL), $1);
                                $$->info = $1->info; }
       | litlst lit           { $$ = binNode(VALUE, $1, $2);
                                $$->info = STRTYPE; }
       ;

lit : NUM                     { $$ = intNode(NUM, $1);
                                $$->info = NUMTYPE; is_null = $1; }
    | CHA                     { $$ = intNode(CHA, $1);
                                $$->info = NUMTYPE; }
    | STR                     { $$ = strNode(STR, $1);
                                $$->info = STRTYPE; }
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
     | leftv '#' expr ';'                              { $$ = binNode(ALLOC, $3, $1); }
     ;

elifs :                                   { $$ = nilNode(NIL); }
      | elifs ELIF expr THEN block        { $$ = binNode(ELIF, $1, binNode(CONDT, $3, $5)); }
      ;

block : insts endinst                      { $$ = binNode(BLOCK, $1, $2); }
      ;

endinst :                                  { $$ = nilNode(NIL); }
        | retn                             { $$ = $1; }
        | REP                              { $$ = nilNode(REP); checkInFor(); }
        | STOP                             { $$ = nilNode(STOP); checkInFor(); }
        ;

retn : RETN                               { $$ = uniNode(RETN, nilNode(NIL)); }
     | RETN expr                          { $$ = uniNode(RETN, $2); }

expr : leftv                              { $$ = $1;
                                            $$->info = $1->info; }
     | litlst                             { $$ = $1;
                                            $$->info = $1->info; }
     | '(' expr ')'                       { $$ = $2;
                                            $$->info = $2->info; }
     | ID '(' args ')'                    { $$ = binNode(CALL, strNode(ID, $1), $3);
                                            $$->info = checkInvocation($1, $3); }
     | ID '(' args ')' '[' expr ']'       { $$ = binNode(CALL, strNode(ID, $1), $3);
                                            $$->info = checkInvocation($1, $3); }
     | '?'                                { $$ = nilNode('?');
                                            $$->info = NUMTYPE; /* What to do here? */}
     | '&' leftv %prec ADDR               { $$ = uniNode(ADDR, $2);
                                            $$->info = checkAddress($2); }
     | '-' expr %prec UMINUS              { $$ = uniNode(UMINUS, $2);
                                            $$->info = checkIfInt($2); }
     | expr '^' expr                      { $$ = binNode('^', $1, $3);
                                            $$->info = checkIntOp($1, $3); }
     | expr '*' expr                      { $$ = binNode('*', $1, $3);
                                            $$->info = checkIntOp($1, $3); }
     | expr '/' expr                      { $$ = binNode('/', $1, $3);
                                            $$->info = checkIntOp($1, $3); }
     | expr '%' expr                      { $$ = binNode('%', $1, $3);
                                            $$->info = checkIntOp($1, $3); }
     | expr '+' expr                      { $$ = binNode('+', $1, $3);
                                            $$->info = checkIntOp($1, $3); }
     | expr '-' expr                      { $$ = binNode('-', $1, $3);
                                            $$->info = checkIntOp($1, $3); }
     | expr '<' expr                      { $$ = binNode('<', $1, $3);
                                            $$->info = checkCompOp($1, $3); }
     | expr '>' expr                      { $$ = binNode('>', $1, $3);
                                            $$->info = checkCompOp($1, $3); }
     | expr LE expr                       { $$ = binNode(LE, $1, $3);
                                            $$->info = checkCompOp($1, $3); }
     | expr GE expr                       { $$ = binNode(GE, $1, $3);
                                            $$->info = checkCompOp($1, $3); }
     | expr '=' expr                      { $$ = binNode('=', $1, $3);
                                            $$->info = checkCompOp($1, $3); }
     | expr NE expr                       { $$ = binNode(NE, $1, $3);
                                            $$->info = checkCompOp($1, $3); }
     | '~' expr                           { $$ = uniNode('~', $2);
                                            $$->info = checkIfInt($2); }
     | expr '&' expr                      { $$ = binNode('&', $1, $3);
                                            $$->info = checkIntOp($1, $3); }
     | expr '|' expr                      { $$ = binNode('|', $1, $3);
                                            $$->info = checkIntOp($1, $3); }
     | leftv ASSOC expr                   { $$ = binNode(ASSOC, $1, $3);
                                            $$->info = checkAssociation($1, $3); }
     ;

leftv : ID                  { $$ = binNode(LEFT_VAL, strNode(ID, $1), nilNode(NIL));
                              $$->info = IDfind($1, 0); }
      | ID '[' expr ']'     { $$ = binNode(LEFT_VAL, strNode(ID, $1), $3);
                              $$->info = checkIndexation($1, $3); }
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

static void checkInFor()
{
  if (!in_for)
    yyerror("ERROR : Can only use instruction inside for loop!");
}

static int checkInvocation(char *id, Node *n2)
{
  /*TODO*/
  return n2->info;
}

static int checkIndexation(char *id, Node *offset)
{
  if (IDfind(id, 0) == NUMTYPE)
    yyerror("ERROR : Cannot index an integer!");
  if (offset->info != NUMTYPE)
    yyerror("ERROR : Offset must be an integer!");
  return NUMTYPE;
}

static int checkAddress(Node *n1)
{
  if (n1->info != NUMTYPE)
    yyerror("ERROR : Can only read address of integer types!");
  return ARRTYPE;
}

static int checkAssociation(Node *n1, Node *n2)
{
  if (n1->info != n2->info && !is_null)
    yyerror("ERROR : Association of diferent types!");
  return n1->info;
}

static int checkIfInt(Node *n1)
{
  if (n1->info != NUMTYPE)
    yyerror("ERROR : Operation apllies only to integers!");
  return NUMTYPE;
}

static int checkIntOp(Node *n1, Node *n2)
{
  if (n1->info != NUMTYPE || n2->info != NUMTYPE)
    yyerror("ERROR : Operation must be between integers!");
  return NUMTYPE;
}

static int checkCompOp(Node *n1, Node *n2)
{
  if ((n1->info == NUMTYPE && n2->info == NUMTYPE) || (n1->info == STRTYPE && n2->info == NUMTYPE))
    return NUMTYPE;
  yyerror("ERROR : Comparison must be between two integers or two strings!");
  return 0;
}
