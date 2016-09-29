#ifndef TREE_H
#define TREE_H



typedef enum {expK, funcK, func_declK, decl_noAssignK,
	      decl_assignK, assignK, ifK, ifElseK, returnK,
	      assertK, RWfunc_declK, repeatK, reorderK, nullK} STMTKind;

typedef enum {firstK, restK, emptyArgK} ARG_LISTKind;

typedef enum {fullK, emptyK} PROGRAMKind;

typedef enum {idK,intconstK,timesK,divK,plusK,minusK, func_callK, dotK,
	      eqCheckK, unknownK, regexK} EXPKind;

typedef enum {declK, nonDeclK} ARG_LISTDeclKind;
 
typedef struct EXP {
  int lineno;
  /* enum {idK,intconstK,timesK,divK,plusK,minusK, func_callK, dotK, eqCheckK, unknownK} kind; */
  EXPKind kind;
  union {
    char *idE;
    int intconstE;
    struct {struct EXP *left; struct EXP *right;} timesE;
    struct {struct EXP *left; struct EXP *right;} divE;
    struct {struct EXP *left; struct EXP *right;} plusE;
    struct {struct EXP *left; struct EXP *right;} minusE;
    struct {struct EXP *left; struct EXP *right;} eqCheckE;
    struct {char *left; char *right;} dotE;
    char *regexE;
    struct FUNC *func_callE;
  } val;
} EXP;


typedef struct PROGRAM {
  int lineno;
  /* enum {fullK, emptyK} kind; */
  PROGRAMKind kind;
  struct PROGRAM *prgrm;
  struct STMT *stmt;
} PROGRAM;

typedef struct STMT {
  int lineno;
  /* enum {expK, funcK, func_declK, decl_noAssignK, */
  /* 	decl_assignK, assignK, ifK, ifElseK, returnK, assertK, RWfunc_declK} kind; */
  STMTKind kind;
  union {
    struct {struct EXP *e;} expE;
    struct {struct FUNC *f;} funcE;
    struct {struct FUNC_DECL *f;} func_declE;
    struct {char *tp; char *id;} decl_noAssignE;
    struct {char *tp; char *id; EXP *assignment;} decl_assignE;
    struct {char *id; EXP *assignment;} assignE;
    struct {struct EXP *e; struct PROGRAM *p1;} ifE;
    struct {struct EXP *e; struct PROGRAM *p1; struct PROGRAM *p2;} ifElseE;
    struct {struct EXP *e; struct PROGRAM *p1;} repeatE;
    struct {struct STMT *s1; struct STMT *s2;} reorderE;
    struct {struct ARG_LIST *shared;
      struct ARG_LIST *reads; struct ARG_LIST *writes; struct FUNC_DECL *f; } RWfunc_declE;
    struct EXP *assertE;
    struct EXP *returnE;
  } val;
} STMT;

typedef struct FUNC {
  int lineno;
  char *id;
  struct ARG_LIST *args;
} FUNC;

typedef struct ARG_LIST {
  int lineno;
  /* enum {firstK, restK, emptyArgK} kind; */
  ARG_LISTKind kind;

  ARG_LISTDeclKind decl_kind;
  union {
    struct {EXP *id; struct ARG_LIST *rest;} firstE;
    struct {struct ARG_LIST *old; EXP *id;} restE;
    struct {char *tp; char *id; struct ARG_LIST *rest;} firstDeclE;
    struct {struct ARG_LIST *old; char *tp; char *id;} restDeclE;
  } val;
} ARG_LIST;

typedef struct FUNC_DECL {
  int lineno;
  char *tp;
  char *id;
  struct ARG_LIST *args;
  PROGRAM *program;
} FUNC_DECL;


/* typedef struct DECL { */
/*   int lineno; */
/*   char *tp; */
/*   char *id; */
/*   enum {noAssignK, assignK} kind; */
/*   EXP *assignment; */
  
/* } DECL; */




PROGRAM *makeProgram(PROGRAM *left, STMT *right);


PROGRAM *copyProgram(PROGRAM *p);



PROGRAM *makeEmptyProgram();


STMT *makeStmtExp(EXP *e);
STMT *makeStmtFuncCall(FUNC *f);
STMT *makeStmtFuncDecl(FUNC_DECL *f);
STMT *makeStmtRWFuncDecl(ARG_LIST *shared, ARG_LIST *reads, ARG_LIST *writes, FUNC_DECL *f);
STMT *makeStmtDecl(char *tp, char *id);
STMT *makeStmtDeclAssign(char *tp, char *id, EXP *assignment);
STMT *makeStmtAssign(char *id, EXP *assignment);
STMT *makeStmtIf(EXP *cond, PROGRAM *p1);
STMT *makeStmtIfElse(EXP *cond, PROGRAM *p1, PROGRAM *p2);
STMT *makeStmtReturn(EXP *e);
STMT *makeStmtAssert(EXP *e);


FUNC *makeFuncCall(char *id, ARG_LIST *l);

ARG_LIST *makeSingleArgList(EXP *id, ARG_LIST *l);
ARG_LIST *makeArgList(ARG_LIST *l, EXP *id);
ARG_LIST *makeEmptyArg();
 

ARG_LIST *makeSingleArgDeclList(char *tp, char *id, ARG_LIST *rest);
ARG_LIST *makeArgDeclList(ARG_LIST *first, char *tp, char *id);
ARG_LIST *makeEmptyDeclArg();

FUNC_DECL *makeFuncDecl(char *tp, char *id, ARG_LIST *args, PROGRAM *p);

EXP *makeExpFuncCall(FUNC *f);

EXP *makeEXPid(char *id);

EXP *makeEXPintconst(int intconst);

EXP *makeEXPtimes(EXP *left, EXP *right);

EXP *makeEXPdiv(EXP *left, EXP *right);
EXP *makeExpUnknown();

EXP *makeEXPplus(EXP *left, EXP *right);
EXP *makeExpEqCheck(EXP *left, EXP *right);
EXP *makeEXPminus(EXP *left, EXP *right);
EXP *makeExpDotAccess(char *left, char *right);

#endif /* !TREE_H */
