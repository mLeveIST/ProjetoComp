%{
/* -----------------------------------------
 * Compiladores 19/20 - Entrega Intermedia
 * Author: Miguel Levezinho, No 90756
 * -----------------------------------------
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "node.h"
#include "tabid.h"

#define YYDEBUG 1

int yylex(), yyerror(char *s), yyparse();
int errors;

static int formatType(int type, int qualifier, int constant, int function);
static int parseType(int idtype);
static int parseQualifier(int idtype);
static int parseConstant(int idtype);
static int parseFunction(int idtype);

static Node *varNode(Node *type, char *id, Node *dim);
static Node *declNode(Node *qualif, Node *cons, Node *var, Node *declval);
static void declareVar(Node *var);
static Node *funcNode(Node *qualif, Node *ftype, char *id, Node *params);
static void checkFuncForward(int forward, int done);

static int checkReturn(Node *n);
static void checkIfExpr(Node *n1);
static void checkInFor();
static void checkForExpr(Node *n1, Node *n2, Node *n3);
static int checkInvocation(char *id, Node *n2);
static int compareArgs(Node *decl, Node *call, int sigErr);
static void checkPrint(Node *n);
static void checkAllocation(Node *n1, Node *n2);
static int checkAddress(Node *n1);
static int checkAssociation(Node *n1, Node *n2);
static int checkInvocation(char *id, Node *n2);
static int checkIndexation(char *id, Node *offset);
static int checkIfInt(Node *n1);
static int checkSumOp(Node *n1, Node *n2);
static int checkSubOp(Node *n1, Node *n2);
static int checkIntOp(Node *n1, Node *n2);
static int checkCompOp(Node *n1, Node *n2);

/* 0 if not inside a for loop ;
 * > 0 for each time the program enters a for block
 * Used to assure proper use of STOP and REPEAT.
 */
int for_lvl;

/* Specifies if the last read literal was not the null pointer
 * Used mostly in association logic verifications.
 */
int not_null;

/* Stores the curr function type,
 * used to match later with return statement.
 */
int func_type;

/* List size, to store the size of chains/array of literals
 * that are read (can be one if only one literal is read)
 * Used mostly in declaration semantic verifications.
 */
int lstSize;

/* Used to store latest read ID value, for semantic
 * error checking in declarations.
 */
Node *auxPtr;

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

%type <n> prog modl opdecls decls decl declval
%type <n> func fbody fhead ftype vars params
%type <n> var dim qualf cons type lit body bvars
%type <n> insts inst elifs block endinst retn
%type <n> expr leftv args lits numlst litlst

%token NIL ERR DECLS DECL VARS VAR LEFT_VAL ARGS CALL
%token BODY BODY_VARS ARRAY INSTS BLOCK ALLOC IFELSE CONDT RANGE
%token MODIFIERS LVAL FTYPE DVAL FBODY FHEAD VVAL FVAL

%%

/* Node tree construction uses only up to binary nodes */

file : prog                   { if (errors == 0) printNode($1, 0, (char**) yyname); freeNode($1); }
     | modl                   { if (errors == 0) printNode($1, 0, (char**) yyname); freeNode($1); }
     ;

/* program */

prog : PROG opdecls START     { IDpush(); func_type = NUMTYPE; }
       body END               { IDpop(); $$ = binNode(PROG, $2, binNode(START, $5, nilNode(END))); }
     ;

/* module */

modl : MODL opdecls END       { $$ = uniNode(MODL, $2); }
     ;

/* optional declarations */

opdecls :                     { $$ = nilNode(NIL); }
        | decls               { $$ = $1; }
        ;

/* declarations (error recovery point) */

decls : decl                  { $$ = binNode(DECLS, nilNode(NIL), $1); }
      | error                 { $$ = binNode(DECLS, nilNode(NIL), nilNode(ERR)); }
      | decls ';' decl        { $$ = binNode(DECLS, $1, $3); }
      | decls ';' error       { $$ = binNode(DECLS, $1, nilNode(ERR)); }
      ;

/* declaration */

decl : func                          { $$ = $1; }
     | qualf cons var declval        { $$ = declNode($1, $2, $3, $4); }
     ;

/* declaration value */

declval :                     { $$ = nilNode(NIL); }
        | ASSOC lits          { $$ = $2; }
        ;

/* function */

func : FUNC fhead fbody       { $$ = binNode(FUNC, $2, $3); IDpop(); func_type = 0;
                                checkFuncForward($2->info, $3->info); }
     ;

/* function body */

fbody : DONE                  { $$ = nilNode(DONE);
                                $$->info = DONE; }
      | DO body retn          { $$ = binNode(FBODY, $2, $3); } 
      ;

/* function signature (IDnew is called during reduction here) */

fhead : qualf ftype ID        { IDpush(); func_type = $2->info; }
        params                { $$ = funcNode($1, $2, $3, $5);
                                $$->info = $1->info; }
      ;

/* function type (adds VOID to type) */

ftype : VOID                  { $$ = nilNode(VOID);
                                $$->info = VOID; }
      | type                  { $$ = $1; }
      ;

/* function parameters */

params :                      { $$ = nilNode(NIL); }
       | vars                 { $$ = $1; }
       ;

/* variables (IDnew is called during reduction here) */

vars : var                    { $$ = binNode(ARGS, nilNode(NIL), $1);
                                $$->info = $1->info; declareVar($1); }
     | vars ';' var           { $$ = binNode(ARGS, $1, $3);
                                $$->info = $3->info; declareVar($3); }
     ;

/* variable */

var : type ID dim             { $$ = varNode($1, $2, $3); }
    ;

/* dimension of variable (for arrays) */

dim :                         { $$ = nilNode(NIL); }
    | '[' NUM ']'             { $$ = intNode(NUM, $2);
                                $$->info = $2; }
    ;

/* qualifier */

qualf :                       { $$ = nilNode(NIL);
                                $$->info = 0; }
      | PUBL                  { $$ = nilNode(PUBL);
                                $$->info = PUBL; }
      | FRWD                  { $$ = nilNode(FRWD);
                                $$->info = FRWD; }
      ;

/* constant */

cons :                        { $$ = nilNode(NIL);
                                $$->info = 0; }
     | CONS                   { $$ = nilNode(CONS);
                                $$->info = CONS; }
     ;

/* variable types */

type : NUMTYPE                { $$ = nilNode(NUMTYPE);
                                $$->info = NUMTYPE; }
     | STRTYPE                { $$ = nilNode(STRTYPE);
                                $$->info = STRTYPE; }
     | ARRTYPE                { $$ = nilNode(ARRTYPE);
                                $$->info = ARRTYPE; }
     ;

/* literals */

lits : numlst                 { $$ = $1;
                                $$->info = ARRTYPE; }
     | litlst                 { $$ = $1; }
     ;

/* number list (array) */

numlst : NUM ',' NUM          { $$ = binNode(ARRAY, intNode(NUM, $1), intNode(NUM, $3));
                                lstSize = 2; not_null = 1; }
       | numlst ',' NUM       { $$ = binNode(ARRAY, $1, intNode(NUM, $3));
                                ++lstSize; }
       ;

/* literal list (number or string) */

litlst : lit                  { $$ = binNode(LVAL, nilNode(NIL), $1);
                                $$->info = $1->info; lstSize = 1; }
       | litlst lit           { $$ = binNode(LVAL, $1, $2);
                                $$->info = STRTYPE; ++lstSize; }
       ;

/* individual literal */

lit : NUM                     { $$ = intNode(NUM, $1);
                                $$->info = NUMTYPE; not_null = $1; }
    | CHA                     { $$ = intNode(CHA, $1);
                                $$->info = NUMTYPE; not_null = 1; }
    | STR                     { $$ = strNode(STR, $1);
                                $$->info = STRTYPE; not_null = 1; }
    ;

/* body (for functions including main ) */

body : bvars insts            { $$ = binNode(BODY, $1, $2); }
     ;

/* body variables */

bvars :                       { $$ = nilNode(NIL); }
      | bvars var ';'         { $$ = binNode(BODY_VARS, $1, $2);
                                declareVar($2); }
      ;

/* instructions (error recovery point) */

insts :                       { $$ = nilNode(NIL); }
      | insts inst            { $$ = binNode(INSTS, $1, $2); }
      | insts error           { $$ = binNode(INSTS, $1, nilNode(ERR)); }
      ;

/* instruction */

inst : IF expr THEN block elifs FI                { $$ = binNode(IF, binNode(CONDT, $2, $4), $5);
                                                    checkIfExpr($2); }
     | IF expr THEN block elifs ELSE block FI     { $$ = binNode(IFELSE, binNode(IF, binNode(CONDT, $2, $4), $5), $7);
                                                    checkIfExpr($2); }
     | FOR expr UNTIL expr STEP expr DO           { for_lvl++; checkForExpr($2, $4, $6); }
       block DONE                                 { $$ = binNode(FOR, binNode(RANGE, $2, binNode(DO, $4, $6)), $9);
                                                    for_lvl--; }
     | expr ';'                                   { $$ = $1; }
     | expr '!'                                   { $$ = uniNode('!', $1);
                                                    checkPrint($1); }
     | leftv '#' expr ';'                         { $$ = binNode(ALLOC, $3, $1);
                                                    checkAllocation($1, $3);}
     ;

/* elif instruction */

elifs :                                   { $$ = nilNode(NIL); }
      | elifs ELIF expr THEN block        { $$ = binNode(ELIF, $1, binNode(CONDT, $3, $5));
                                            checkIfExpr($3); }
      ;

/* block (inside ifs or for loops) */

block : insts endinst             { $$ = binNode(BLOCK, $1, $2); }
      ;

/* ending instructions (for blocks) */

endinst : retn                    { $$ = $1; }
        | REP                     { $$ = nilNode(REP); checkInFor(); }
        | STOP                    { $$ = nilNode(STOP); checkInFor(); }
        ;

/* optional return instruction (no flow control) */

retn :                            { $$ = nilNode(NIL); }
     | RETN                       { $$ = uniNode(RETN, nilNode(NIL));
                                    $$->info = checkReturn(0); }
     | RETN expr                  { $$ = uniNode(RETN, $2);
                                    $$->info = checkReturn($2); }
     ;

/* expression */

expr : leftv                      { $$ = $1;
                                    $$->info = parseType($1->info); }
     | litlst                     { $$ = $1; }
     | '(' expr ')'               { $$ = $2; }
     | ID '(' args ')'            { $$ = binNode(CALL, strNode(ID, $1), $3);
                                    $$->info = checkInvocation($1, $3); }
     | '?'                        { $$ = nilNode('?');
                                    $$->info = NUMTYPE; }
     | '&' leftv %prec ADDR       { $$ = uniNode(ADDR, $2);
                                    $$->info = checkAddress($2); }
     | '-' expr %prec UMINUS      { $$ = uniNode(UMINUS, $2);
                                    $$->info = checkIfInt($2); }
     | expr '^' expr              { $$ = binNode('^', $1, $3);
                                    $$->info = checkIntOp($1, $3); }
     | expr '*' expr              { $$ = binNode('*', $1, $3);
                                    $$->info = checkIntOp($1, $3); }
     | expr '/' expr              { $$ = binNode('/', $1, $3);
                                    $$->info = checkIntOp($1, $3); }
     | expr '%' expr              { $$ = binNode('%', $1, $3);
                                    $$->info = checkIntOp($1, $3); }
     | expr '+' expr              { $$ = binNode('+', $1, $3);
                                    $$->info = checkSumOp($1, $3); }
     | expr '-' expr              { $$ = binNode('-', $1, $3);
                                    $$->info = checkSubOp($1, $3); }
     | expr '<' expr              { $$ = binNode('<', $1, $3);
                                    $$->info = checkCompOp($1, $3); }
     | expr '>' expr              { $$ = binNode('>', $1, $3);
                                    $$->info = checkCompOp($1, $3); }
     | expr LE expr               { $$ = binNode(LE, $1, $3);
                                    $$->info = checkCompOp($1, $3); }
     | expr GE expr               { $$ = binNode(GE, $1, $3);
                                    $$->info = checkCompOp($1, $3); }
     | expr '=' expr              { $$ = binNode('=', $1, $3);
                                    $$->info = checkCompOp($1, $3); }
     | expr NE expr               { $$ = binNode(NE, $1, $3);
                                    $$->info = checkCompOp($1, $3); }
     | '~' expr                   { $$ = uniNode('~', $2);
                                    $$->info = checkIfInt($2); }
     | expr '&' expr              { $$ = binNode('&', $1, $3);
                                    $$->info = checkIntOp($1, $3); }
     | expr '|' expr              { $$ = binNode('|', $1, $3);
                                    $$->info = checkIntOp($1, $3); }
     | leftv ASSOC expr           { $$ = binNode(ASSOC, $1, $3);
                                    $$->info = checkAssociation($1, $3); }
     ;

/* left-value */

leftv : ID                  { $$ = binNode(LEFT_VAL, strNode(ID, $1), nilNode(NIL));
                              $$->info = IDfind($1, 0); }
      | ID '[' expr ']'     { $$ = binNode(LEFT_VAL, strNode(ID, $1), $3);
                              $$->info = checkIndexation($1, $3); }
      ;

/* arguments (to call function) */

args : expr                 { $$ = binNode(ARGS, nilNode(NIL), $1);
                              $$->info = $1->info; }
     | args ',' expr        { $$ = binNode(ARGS, $1, $3);
                              $$->info = $3->info; }
     ;

%%

char **yynames =
#if YYDEBUG > 0
  (char**)yyname;
#else
  0;
#endif

/* Return parse tree branch that represents a partial
 * declaration of a global variable.
 * Checks the proper use of the dimension declaration.
 */
static Node *varNode(Node *type, char *id, Node *dim)
{
  Node *var = binNode(VAR, binNode(VVAL, type, auxPtr = strNode(ID, id)), dim);
  var->info = type->info;

  if (type->info != ARRTYPE && dim->info != -1) {
    yyerror("ERROR : Can only define dimension for array type!");
    var->info = -1;
  }
  else if (type->info == ARRTYPE && dim->info == 0) {
    yyerror("ERROR : Cannot define array with size 0!");
    var->info = -1;
  }

  return var;
}

/* Return parse tree branch that represents a global variable declaration.
 * Also adds an entry in the symbol table for this variable only if the
 * declaration is valid.
 * If not, can throw multiple errors depending on what failed.
 *
 * Errors are throw depending on priority, and only one is thrown!
 */
static Node *declNode(Node *qualif, Node *cons, Node *var, Node *declval)
{
  void *attrib;
  char *id;
  int idtype, qualifier, dim;

  id = auxPtr->value.s;

  if (var->info != -1) {
    if ((idtype = IDfind(id, (void**) IDtest)) != -1) {
      if (IDfind(id, &attrib) && attrib != 0)
        yyerror("ERROR : Variable name already taken by a function!");
      else if ((qualifier = parseQualifier(idtype)) != FRWD)
        yyerror("ERROR : Cannot redefine not forwarded variable!");
      else {
        idtype = formatType(var->info, qualif->info, cons->info, 0);
        IDchange(idtype, id, 0, 0);
      }
    }
    else if (qualif->info == FRWD && declval->info != -1)
      yyerror("ERROR : Forwarded variable cannot have a body!");
    else if ((dim = var->CHILD(1)->info) > 0 && dim < lstSize)
      yyerror("ERROR : Number of integers exceeds specified dimension for array!");
    else if (dim > 0 && lstSize == 1 && var->info == STRTYPE)
      yyerror("ERROR : Arrays can only be initialized with integer sequences!");
    else if (qualif->info != FRWD && cons->info == CONS && declval->info == -1)
      yyerror("ERROR : Uninitialized constant variable!");
    else if (declval->info != -1 && dim < 1) {
      if (var->info == ARRTYPE && dim == -1)
        yyerror("ERROR : Connot initialize array that has no dimension!");
      else if (var->info != NUMTYPE && !not_null)
        yyerror("ERROR : Declaration cannot be initialized with a null pointer!");
      else if (declval->info != var->info)
        yyerror("ERROR : Initialized value does not match declared variable type!");
      else
        idtype = formatType(var->info, qualif->info, cons->info, 0);
        IDnew(idtype, id, 0);
    }
    else {
      idtype = formatType(var->info, qualif->info, cons->info, 0);
      IDnew(idtype, id, 0);
    }
  }

  return binNode(DECL, binNode(MODIFIERS, qualif, cons), binNode(DVAL, var, declval));
}

/* Function that creates an entry in the symbol table for the passed
 * variable (for the case os parameters and body variables).
 * May throw an error if the variable is already defined and no
 * overriding is available.
 */
static void declareVar(Node *var)
{
  int idtype;
  char *id = auxPtr->value.s;

  if (var->info != -1) {
    if ((idtype = IDsearch(id, (void**) IDtest, 0, 1)) != -1)
      yyerror("ERROR : Variable already defined within block!");
    else if ((idtype = IDsearch(id, (void**) IDtest, 1, 1)) != -1)
      IDreplace(formatType(var->info, 0, 0, 0), id, 0);
    else
      IDnew(formatType(var->info, 0, 0, 0), id, 0);
  }
}

/* Return parse tree branch that represents a function declaration.
 * Adds an entry in the symbol table for this function only if the
 * declaration is valid. If not, throws an error depending on what went wrong.
 */
static Node *funcNode(Node *qualif, Node *ftype, char *id, Node *params)
{
  Node *attrib;
  int idtype, qualifier;

  if ((idtype = IDsearch(id, (void**) IDtest, 1, 1)) != -1) {
    if (parseFunction(idtype) != FUNC)
      yyerror("ERROR : Function name already taken by a variable!");
    else if ((qualifier = parseQualifier(idtype)) != FRWD)
      yyerror("ERROR : Cannot redefine not forwarded functions!");
    else if (qualifier == FRWD) {
      IDsearch(id, (void**) &attrib, 1, 1);
      if (ftype->info != parseType(idtype))
        yyerror("ERROR : Implementation of forward function has wrong type!");
      else if (compareArgs(params, attrib, 0))
        yyerror("ERROR : Implementation of forward function arguments do not match!");
    }
    else
      idtype = formatType(ftype->info, qualif->info, 0, 1);
      IDchange(idtype, id, (void *) params, 1);
  }
  else {
    idtype = formatType(ftype->info, qualif->info, 0, 1);
    IDinsert(0, idtype, id, (void *) params);
  }

  return binNode(FHEAD, qualif, binNode(FTYPE, binNode(FVAL, ftype, strNode(ID, id)), params));
}

/* Verifies if the declaration of a forward function is completed.
 */
static void checkFuncForward(int forward, int done)
{
  if (forward == FRWD && done != DONE)
    yyerror("ERROR : Forwarded function cannot have a body!");
  if (forward != FRWD && done == DONE)
    yyerror("ERROR : Not fowarded function missing definition!");
}

/* Formats information about the declaration of a variable into 6 bits,
 * called the idtype, to store as the ID in the symbol table.
 * From less to most significative:
 * - 1 bit to store if ID representes a function or a variable
 * - 1 bit to store if the declaration is constant
 * - 2 bits to store the value of the qualifier
 * - 2 bits to store the value of the type
 * An aditional error bit may be used in parsing functions to determine
 * if input is indeed a valid ID type.
 */
static int formatType(int type, int qualifier, int constant, int function)
{
  int idtype = 0;

  switch (type) {
    case VOID: idtype = 0; break;
    case NUMTYPE: idtype = 1; break;
    case ARRTYPE: idtype = 2; break;
    case STRTYPE: idtype = 3; break;
    default: return -1;
  }
  idtype = idtype << 2;

  switch (qualifier) {
    case 0: break;
    case PUBL: idtype += 1; break;
    case FRWD: idtype += 2; break;
    default: return -1;
  }
  idtype = (idtype << 1) + (constant ? 1 : 0);

  return (idtype << 1) + (function ? 1 : 0); 
}

/* Recovers the type of an ID from the idtype.
 */
static int parseType(int idtype)
{
  if (idtype == VOID || idtype == NUMTYPE || idtype == ARRTYPE || idtype == STRTYPE)
    return idtype;

  switch (idtype >> 4) {
    case 0: return VOID;
    case 1: return NUMTYPE;
    case 2: return ARRTYPE;
    case 3: return STRTYPE;
    default: return -1;
  }
}

/* Recovers the qualifier of an ID from the idtype.
 */
static int parseQualifier(int idtype)
{
  if (idtype == VOID || idtype == NUMTYPE || idtype == ARRTYPE || idtype == STRTYPE)
    return 0;

  switch ((idtype >> 2) & 19) {
    case 0: return 0;
    case 1: return PUBL;
    case 2: return FRWD;
    default: return -1;
  }
}

/* Recovers from the idtype if the ID is constant.
 */
static int parseConstant(int idtype)
{
  if (idtype == VOID || idtype == NUMTYPE || idtype == ARRTYPE || idtype == STRTYPE)
    return 0;

  switch ((idtype >> 1) & 33) {
    case 0: return 0;
    case 1: return CONS;
    default: return -1;
  }
}

/* Recovers the from the idtype if the ID is from a function.
 */
static int parseFunction(int idtype)
{
  if (idtype == VOID || idtype == NUMTYPE || idtype == ARRTYPE || idtype == STRTYPE)
    return 0;

  switch (idtype & 65) {
    case 0: return 0;
    case 1: return FUNC;
    default: return -1;
  }
}

/* Check function that verifies if the return expression type
 * is the same as the type declared in the function signature.
 */
static int checkReturn(Node *n)
{
  if (n == 0 && func_type != VOID) {
    yyerror("ERROR : Missing return expression!");
    return -1;
  }
  else if (n != 0) {
    if (n->info == VOID) {
      yyerror("ERROR : Invalid return type for function!");
      return -1;
    }
    else if (n->info != func_type) {
      yyerror("ERROR : Return type does not match function type!");
      return -1;
    }
  }
  return n == 0 ? VOID : n->info;
}

/* Check function that verifies if the proper use of
 * STOP and REPEAT operations, inside a for loop.
 */
static void checkInFor()
{
  if (!for_lvl)
    yyerror("ERROR : Can only use instruction inside for loop!");
}

/* Check function that verifies that the passed expression
 * can be printed by the ! instruction.
 */
static void checkPrint(Node *n)
{
  if (n->info == VOID)
    yyerror("ERROR : Cannot print void expression!");
}

/* Check function that verifies the proper types and
 * qualities of variables to be able to reserve memory.
 */
static void checkAllocation(Node *lv, Node *n2)
{
  int type = parseType(lv->info);
  int cons = parseConstant(lv->info);

  if (type == NUMTYPE)
    yyerror("ERROR : Can only reserve memory for pointer types!");
  if (cons)
    yyerror("ERROR : Cannot alloc memory for constant variable!");
}

/* Verifies the correctness and completeness of a function
 * invocation, in terms of type and arguments.
 * Uses the compareArgs function to compare
 * declared parameters and call arguments.
 */
static int checkInvocation(char *id, Node *args)
{
  Node *params;
  int idtype = IDfind(id, (void**) &params);

  if (parseFunction(idtype) != FUNC)
    yyerror("ERROR : Cannot invoke ID that is not a function!");
  else
    compareArgs(params, args, 1);

  return parseType(idtype);
}

/* Function that moves down recursively through two sub-trees
 * of function arguments (ARGS) and checks if they are of the
 * same size and type.
 * sigErr is a flag to enable/disable error throwing during the search.
 */
static int compareArgs(Node *decl, Node *call, int sigErr)
{
  int i;

  if (decl->attrib == ARGS && call->attrib == ARGS)
    for (i = 0; i < decl->value.sub.num; i++) {
      if (compareArgs(decl->value.sub.n[i], call->value.sub.n[i], sigErr) != 0)
        return -1;
      if (call->info != decl->info) {
        if (sigErr) yyerror("ERROR : Argument type mismatch in function call!");
        return -1;
      }
    }
  else if (decl->attrib == ARGS) {
    if (sigErr) yyerror("ERROR : Missing arguments in function call!");
    return -1;
  }
  else if (call->attrib == ARGS) {
    if (sigErr) yyerror("ERROR : Too many arguments in function call!");
    return -1;
  }
  return 0;
}

/* Check function of the expressions used in conditional if and elif.
 */
static void checkIfExpr(Node *n1)
{
  if (n1->info == VOID) 
    yyerror("ERROR : Conditional expression must return an integer!");
}

/* Check function of the expressions used in for cycles.
 */
static void checkForExpr(Node *n1, Node *n2, Node *n3)
{
  if (n1->info == VOID)
    yyerror("ERROR : First expression in cycle must return an integer!");
  if (n2->info == VOID)
    yyerror("ERROR : Stop condition in cycle must return an integer!");
  if (n3->info == VOID)
    yyerror("ERROR : Step condition in cycle must return an integer!");
}

/* Check function that verifies proper indexation of variables.
 * Index of a function is not allowed in this implementation.
 */
static int checkIndexation(char *id, Node *offset)
{
  int idtype = IDfind(id, 0);

  if (parseFunction(idtype) == FUNC)
    yyerror("ERROR : Cannot index a function!");
  else if (parseType(idtype) == NUMTYPE)
    yyerror("ERROR : Cannot index an integer!");
  else if (offset->info != NUMTYPE)
    yyerror("ERROR : Offset must be an integer!");
  return NUMTYPE;
}

/* Check function that verifies proper use the
 * memory read operator.
 */
static int checkAddress(Node *lv)
{
  if (parseType(lv->info) != NUMTYPE)
    yyerror("ERROR : Can only read address of integer types!");
  return ARRTYPE;
}

/* Check function that verifies proper use the association
 * expression.
 * Uses the not_null global var to check cases of null pointer
 * assignment to array or string types.
 */
static int checkAssociation(Node *lv, Node *n1)
{
  int type;

  if (lv->info == -1)
    return -1;

  if (parseFunction(lv->info) == FUNC)
    yyerror("ERROR : Cannot assign a value to a function!");
  else if (parseConstant(lv->info) == CONS)
    yyerror("ERROR : Cannot assign a value to a constant variable!");
  else if ((type = parseType(lv->info)) != n1->info && not_null)
    yyerror("ERROR : Association between different types!");
  not_null = 1;
  return type;
}

/* Check function to verify if the given expression results
 * in a type integer.
 */
static int checkIfInt(Node *n1)
{
  if (n1->info != NUMTYPE)
    yyerror("ERROR : Operation applies only to integers!");
  return NUMTYPE;
}

/* Check function to verify if the aritmetic sum is being used
 * with the correct operands.
 * Pointer aritmetic is allowed.
 */
static int checkSumOp(Node *n1, Node *n2)
{
  if (n1->info == NUMTYPE && n2->info == NUMTYPE)
    return NUMTYPE;
  else if (n1->info == STRTYPE || n2->info == STRTYPE)
    yyerror("ERROR : Sum operation does not apply to string values!");
  else if (n1->info == ARRTYPE && n2->info == ARRTYPE)
    yyerror("ERROR : Cannot sum to vector types!");
  return ARRTYPE;
}

/* Check function to verify if the subtraction is being used
 * with the correct operands.
 * Pointer aritmetic is allowed.
 */
static int checkSubOp(Node *n1, Node *n2)
{
  if (n1->info == n2->info && n1->info != STRTYPE)
    return NUMTYPE;
  else if (n1->info == STRTYPE || n2->info == STRTYPE)
    yyerror("ERROR : Subtraction does not apply to string values!");
  return ARRTYPE;
}

/* Check function to verify if the operation is between integers.
 */
static int checkIntOp(Node *n1, Node *n2)
{
  if (n1->info != NUMTYPE || n2->info != NUMTYPE)
    yyerror("ERROR : Operation must be between integers!");
  return NUMTYPE; 
}

/* Check function to verify if the comparison expresions
 * are made only between integers and strings.
 */
static int checkCompOp(Node *n1, Node *n2)
{
  if ((n1->info != NUMTYPE || n2->info != NUMTYPE) && (n1->info != STRTYPE || n2->info != STRTYPE))
    yyerror("ERROR : Comparison must be between two integers or two strings!");
  return NUMTYPE;
}
