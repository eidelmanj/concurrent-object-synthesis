#include "stdio.h"
#include "stdlib.h"
#include "memory.h"
#include "tree.h"
 
extern int lineno;




PROGRAM *makeProgram(PROGRAM *left, STMT *right) {
  PROGRAM *p;
  p = NEW(PROGRAM);
  p->lineno = lineno;
  p->kind = fullK;
  p->prgrm = left;
  p->stmt = right;
  return p;
}





PROGRAM *makeEmptyProgram() {
  PROGRAM *p;
  p = NEW(PROGRAM);
  p->lineno = lineno;
  p->kind = emptyK;
  return p;
}



STMT *makeStmtExp(EXP *e) {
  STMT *s;
  s = NEW(STMT);
  s->lineno = lineno;
  s->kind = expK;
  s->val.expE.e = e;
  return s;
}

STMT *makeStmtFuncCall(FUNC *f) {
  STMT *s;
  s = NEW(STMT);
  s->lineno = lineno;
  s->kind = funcK;
  s->val.funcE.f = f;
  return s;
}

STMT *makeStmtFuncDecl(FUNC_DECL *f) {
  STMT *s;
  s = NEW(STMT);
  s->lineno = lineno;
  s->kind = func_declK;
  s->val.func_declE.f = f;
  return s;
}

STMT *makeStmtRWFuncDecl(ARG_LIST *shared, ARG_LIST *reads, ARG_LIST *writes, FUNC_DECL *f) {
  STMT *s;
  s = NEW(STMT);
  s->lineno = lineno;
  s->kind = RWfunc_declK;
  s->val.RWfunc_declE.shared = shared;
  s->val.RWfunc_declE.reads = reads;
  s->val.RWfunc_declE.writes = writes;
  s->val.RWfunc_declE.f = f;
  return s;
}

STMT *makeStmtDecl(char *tp, char *id) {
  STMT *s;
  s = NEW(STMT);
  s->lineno = lineno;
  s->kind = decl_noAssignK;
  s->val.decl_noAssignE.tp = tp;
  s->val.decl_noAssignE.id = id;
  return s;
}

STMT *makeStmtDeclAssign(char *tp, char *id, EXP *assignment) {
  STMT *s;
  s = NEW(STMT);
  s->lineno = lineno;
  s->kind = decl_assignK;
  s->val.decl_assignE.tp = tp;
  s->val.decl_assignE.id = id;
  s->val.decl_assignE.assignment = assignment;
  return s;
}

STMT *makeStmtAssign(char *id, EXP *assignment) {
  STMT *s;
  s = NEW(STMT);
  s->lineno = lineno;
  s->kind = assignK;
  s->val.assignE.id = id;
  s->val.assignE.assignment = assignment;
  return s;
}


STMT *makeStmtIf(EXP *e, PROGRAM *p1) {
  STMT *s;
  s = NEW(STMT);
  s->lineno = lineno;
  s->kind = ifK;
  s->val.ifE.e = e;
  s->val.ifE.p1 = p1;
  return s;
}

STMT *makeStmtIfElse(EXP *e, PROGRAM *p1, PROGRAM *p2) {
  STMT *s;
  s = NEW(STMT);
  s->lineno = lineno;
  s->kind = ifElseK;
  s->val.ifElseE.e = e;
  s->val.ifElseE.p1 = p1;
  s->val.ifElseE.p2 = p2;
  return s;
}


STMT *makeStmtReturn(EXP *e) {
  STMT *s;
  s = NEW(STMT);
  s->lineno = lineno;
  s->kind = returnK;
  s->val.returnE = e;
  return s;
}

STMT *makeStmtAssert(EXP *e) {
  STMT *s;
  s = NEW(STMT);
  s->lineno = lineno;
  s->kind = assertK;
  s->val.assertE = e;
  return s;
}

FUNC *makeFuncCall(char *id, ARG_LIST *l) {
  FUNC *f;
  f = NEW(FUNC);
  f->lineno = lineno;
  f->id = id;
  f->args = l;
  return f;
}

FUNC_DECL *makeFuncDecl(char *tp, char *id, ARG_LIST *args, PROGRAM *p) {
  FUNC_DECL *f;
  f = NEW(FUNC_DECL);
  f->lineno = lineno;
  f->tp = tp;
  f->id = id;
  f->args = args;
  f->program = p;
  return f;
}

ARG_LIST *makeEmptyArg() {
  ARG_LIST *l;
  l = NEW(ARG_LIST);

  l->lineno = lineno;
  l->kind = emptyArgK;
  l->decl_kind = nonDeclK;
  return l;
}


ARG_LIST *makeEmptyDeclArg() {
  ARG_LIST *l;
  l = NEW(ARG_LIST);
  l->lineno = lineno;
  l->kind = emptyArgK;
  l->decl_kind = declK;
  return l;
}

ARG_LIST *makeArgDeclList(ARG_LIST *first, char *tp, char *id) {
  ARG_LIST *l;
  l = NEW(ARG_LIST);
  l->lineno = lineno;
  l->kind = restK;
  l->decl_kind = declK;
  l->val.restDeclE.old = first;
  l->val.restDeclE.id = id;
  l->val.restDeclE.tp = tp;
  return l;
}

ARG_LIST *makeSingleArgDeclList(char *tp, char *id, ARG_LIST *rest) {
  ARG_LIST *l;
  l = NEW(ARG_LIST);
  l->lineno = lineno;
  l->kind = firstK;
  l->decl_kind = declK;
  l->val.firstDeclE.rest = rest;
  l->val.firstDeclE.id = id;
  l->val.firstDeclE.tp = tp;
  return l;
}

ARG_LIST *makeSingleArgList(EXP *id, ARG_LIST *rest) {
  ARG_LIST *l;
  l= NEW(ARG_LIST);
  l-> lineno = lineno;
  l->kind = firstK;
  l->decl_kind = nonDeclK;
  l->val.firstE.rest = rest;
  l->val.firstE.id = id;
  return l;
}

ARG_LIST *makeArgList(ARG_LIST *first, EXP *id) {
  ARG_LIST *l;
  l = NEW(ARG_LIST);
  l-> lineno = lineno;
  l->kind = restK;
  l->decl_kind = nonDeclK;
  l->val.restE.old = first;
  l->val.restE.id = id;
  return l;
}


EXP *makeExpFuncCall(FUNC *f) {
  EXP *e;
  e = NEW(EXP);
  e->lineno = lineno;
  e->kind = func_callK;
  e->val.func_callE = f;
  return e;
}

EXP *makeEXPid(char *id)
{ EXP *e;
  e = NEW(EXP);
  e->lineno = lineno;
  e->kind = idK;
  e->val.idE = id;
  return e;
}

EXP *makeEXPintconst(int intconst)
{ EXP *e;
  e = NEW(EXP);
  e->lineno = lineno;
  e->kind = intconstK;
  e->val.intconstE = intconst;

  return e;
}

EXP *makeEXPtimes(EXP *left, EXP *right)
{ EXP *e;
  e = NEW(EXP);
  e->lineno = lineno;
  e->kind = timesK;
  e->val.timesE.left = left;
  e->val.timesE.right = right;
  return e;
}

EXP *makeEXPdiv(EXP *left, EXP *right)
{ EXP *e;
  e = NEW(EXP);
  e->lineno = lineno;
  e->kind = divK;
  e->val.divE.left = left;
  e->val.divE.right = right;
  return e; 
}

EXP *makeEXPplus(EXP *left, EXP *right)
{ EXP *e;
  e = NEW(EXP);
  e->lineno = lineno;
  e->kind = plusK;
  e->val.plusE.left = left;
  e->val.plusE.right = right;
  return e;
}

EXP *makeEXPminus(EXP *left, EXP *right)
{ EXP *e;
  e = NEW(EXP);
  e->lineno = lineno;
  e->kind = minusK;
  e->val.minusE.left = left;
  e->val.minusE.right = right;
  return e;
}

EXP *makeExpEqCheck(EXP *left, EXP *right) {
  EXP *e;
  e = NEW(EXP);
  e->lineno = lineno;
  e->kind = eqCheckK;
  e->val.eqCheckE.left = left;
  e->val.eqCheckE.right = right;
  return e;
}

EXP *makeExpDotAccess(char *left, char *right) {
  EXP *e;
  e = NEW(EXP);
  e->lineno = lineno;
  e->kind = dotK;
  e->val.dotE.left = left;
  e->val.dotE.right = right;
  return e;
}


EXP *makeExpUnknown() {
  EXP *e;
  e= NEW(EXP);
  e->lineno = lineno;
  e->kind = unknownK;
  return e;
}



/******** Program destructor *******/



/* PROGRAM *copyProgram(PROGRAM *p) { */
/*   PROGRAM *newP = NEW(PROGRAM); */
/*   newP->kind = p->kind; */
/*   newP->program = copyProgram(p->program); */
/*   newP->stmt = copyStmt(p->stmt); */
/*   return newP; */
/* } */
