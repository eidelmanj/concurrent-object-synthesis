#include "tree.h"
#include "tree_interface.hh"
#include "stdlib.h"
#include <cstring>
#include "external_enums.hh"
#include <iostream>

using namespace std;

/***** C++ Destructor **********/
// TODO: Memory leak from (char *)'s which are not freed
void freeProgram(PROGRAM *p) {

  switch (p->kind) {
  case fullK:
    // // std::cout  << "attempting to free stmt\n";
    freeStmt(p->stmt);
    // // std::cout  << "success! Attempting prgrm\n";
    freeProgram(p->prgrm);
    // // std::cout  << "success! Freeing program itself\n";
    break;
  case emptyK:
    // // std::cout  << "Empty! Nothing to free\n";
    break;
  }
  free(p);
}

void freeStmt(STMT *s) {
  switch (s->kind) {
  case expK:
    freeExp(s->val.expE.e);
    break;
  case funcK:
    freeFunc(s->val.funcE.f);
    break;
  case func_declK:
    freeFunc_decl(s->val.func_declE.f);
    break;
  
  case decl_noAssignK:
    break;

  case decl_assignK:
    freeExp(s->val.decl_assignE.assignment);
    break;
  case assignK:
    freeExp(s->val.assignE.assignment);
    break;

  case ifK:
    freeExp(s->val.ifE.e);
    freeProgram(s->val.ifE.p1);
    break;

  case ifElseK:
    freeExp(s->val.ifElseE.e);
    freeProgram(s->val.ifElseE.p1);
    freeProgram(s->val.ifElseE.p2);
    break;

  case RWfunc_declK:
    freeArg_list(s->val.RWfunc_declE.shared);
    freeArg_list(s->val.RWfunc_declE.reads);
    freeArg_list(s->val.RWfunc_declE.writes);
    freeFunc_decl(s->val.RWfunc_declE.f);
    break;

  case assertK:
    freeExp(s->val.assertE);
    break;

  case returnK:
    freeExp(s->val.returnE);
    break;

  case repeatK:
    freeExp(s->val.repeatE.e);
    freeProgram(s->val.repeatE.p1);
    break;
  }
  // // std::cout  << "about to free statement\n";
  free(s);


    
    
}


void freeExp(EXP *e) {
  switch(e->kind) {
  case timesK:
    freeExp(e->val.timesE.left);
    freeExp(e->val.timesE.right);
    break;

  case divK:
    freeExp(e->val.divE.left);
    freeExp(e->val.divE.right);
    break;
  case plusK:
    freeExp(e->val.plusE.left);
    freeExp(e->val.plusE.right);
    break;
  case minusK:
    freeExp(e->val.minusE.left);
    freeExp(e->val.minusE.right);
    break;

  case eqCheckK:
    freeExp(e->val.eqCheckE.left);
    freeExp(e->val.eqCheckE.right);
    break;

  case func_callK:
    freeFunc(e->val.func_callE);
    break;

  case regexK:
    break;

  }


  free(e);
}

void freeArg_list(ARG_LIST *l) {
  switch(l->kind) {
  case emptyArgK:
    break;
    
  case firstK:
    switch(l->decl_kind) {
    case declK:
      freeArg_list(l->val.firstDeclE.rest);
      break;
    case nonDeclK:
      freeExp(l->val.firstE.id);
      freeArg_list(l->val.firstE.rest);
      break;
    }
    break;

  case restK:
    switch(l->decl_kind) {
    case declK:
      freeArg_list(l->val.restDeclE.old);
      break;
    case nonDeclK:
      freeExp(l->val.restE.id);
      freeArg_list(l->val.restE.old);
      break;
    }
    break;
  }
  free(l);
  
}

void freeFunc_decl(FUNC_DECL *f) {
  freeArg_list(f->args);
  freeProgram(f->program);
}

void freeFunc(FUNC *f) {
  freeArg_list(f->args);
  free(f);
}




PROGRAM *copyProgram(PROGRAM *p) {
  PROGRAM *newP = new PROGRAM;
  newP->kind = p->kind;
  switch (p->kind) {
  case fullK:
    // // std::cout  << "Copying statement\n";
    newP->stmt = copyStmt(p->stmt);
    // // std::cout  << "Success! Copying program\n";
    newP->prgrm = copyProgram(p->prgrm);
    // // std::cout  << "success!\n";
    break;
  case emptyK:
    // // std::cout  << "Empty program, nothing to do!\n";
    break;
  }
  return newP;
}




STMT *copyStmt(STMT *s) {
  STMT *newS = new STMT;

  newS->kind = s->kind;
  switch (s->kind) {
  case expK:
    newS->val.expE.e = copyExp(s->val.expE.e);
    break;
  case funcK:
    newS->val.funcE.f = copyFunc(s->val.funcE.f);
    break;
  case func_declK:
    newS->val.func_declE.f = copyFunc_decl(s->val.func_declE.f);
    break;
  
  case decl_noAssignK:
    newS->val.decl_noAssignE.tp = (char *)malloc(strlen(s->val.decl_noAssignE.tp));
    strcpy(newS->val.decl_noAssignE.tp, s->val.decl_noAssignE.tp);

    newS->val.decl_noAssignE.id = (char *) malloc(strlen(s->val.decl_noAssignE.id));
    strcpy(newS->val.decl_noAssignE.id, s->val.decl_noAssignE.id);
    break;

  case decl_assignK:
    newS->val.decl_assignE.tp = (char *)malloc(strlen(s->val.decl_assignE.tp));
    strcpy(newS->val.decl_assignE.tp, s->val.decl_assignE.tp);

    newS->val.decl_assignE.id = (char *) malloc(strlen(s->val.decl_assignE.id));
    strcpy(newS->val.decl_assignE.id, s->val.decl_assignE.id);
    
    newS->val.decl_assignE.assignment = copyExp(s->val.decl_assignE.assignment);
    break;

  case assignK:
    newS->val.assignE.id = (char *) malloc(strlen(s->val.assignE.id));
    strcpy(newS->val.assignE.id, s->val.assignE.id);
    
    newS->val.assignE.assignment = copyExp(s->val.assignE.assignment);
    break;

  case ifK:
    newS->val.ifE.e = copyExp(s->val.ifE.e);
    newS->val.ifE.p1 = copyProgram(s->val.ifE.p1);
    break;

  case ifElseK:
    newS->val.ifElseE.e = copyExp(s->val.ifElseE.e);
    newS->val.ifElseE.p1 = copyProgram(s->val.ifElseE.p1);
    newS->val.ifElseE.p2 = copyProgram(s->val.ifElseE.p2);
    break;

  case RWfunc_declK:
    newS->val.RWfunc_declE.shared = copyArg_list(s->val.RWfunc_declE.shared);
    newS->val.RWfunc_declE.reads = copyArg_list(s->val.RWfunc_declE.reads);
    newS->val.RWfunc_declE.writes = copyArg_list(s->val.RWfunc_declE.writes);
    newS->val.RWfunc_declE.f = copyFunc_decl(s->val.RWfunc_declE.f);

    break;

  case assertK:
    newS->val.assertE = copyExp(s->val.assertE);
    break;

  case returnK:
    newS->val.returnE = copyExp(s->val.returnE);
    break;

  case repeatK:
    newS->val.repeatE.e = copyExp(s->val.repeatE.e);
    newS->val.repeatE.p1 = copyProgram(s->val.repeatE.p1);
    break;

  case reorderK:
    newS->val.reorderE.s1 = copyStmt(s->val.reorderE.s1);
    newS->val.reorderE.s2 = copyStmt(s->val.reorderE.s2);
    break;


  }
  
  return newS;
}


EXP *copyExp(EXP *e) {
  EXP *newE = new EXP;
  // std::cout  << "trying copy exp\n";
  newE->kind = e->kind;
  // std::cout  << "copied kind...\n";

  switch(e->kind) {
  case idK:
    // std::cout  << "idK\n";
    newE->val.idE = (char *) malloc(strlen(e->val.idE));
    strcpy(newE->val.idE, e->val.idE);
    break;

  case intconstK:
    // std::cout  << "intconst\n";
    newE->val.intconstE = e->val.intconstE;
    break;

  case timesK:
    // std::cout  << "times\n";
    newE->val.timesE.left = copyExp(e->val.timesE.left);
    newE->val.timesE.right = copyExp(e->val.timesE.right);
    break;

  case divK:
    // std::cout  << "div\n";
    newE->val.divE.left = copyExp(e->val.divE.left);
    newE->val.divE.right = copyExp(e->val.divE.right);
    break;

  case plusK:
    // std::cout  << "plus\n";
    newE->val.plusE.left = copyExp(e->val.plusE.left);
    newE->val.plusE.right = copyExp(e->val.plusE.right);
    break;

  case minusK:
    // std::cout  << "minus\n";
    newE->val.minusE.left = copyExp(e->val.minusE.left);
    newE->val.minusE.right = copyExp(e->val.minusE.right);
    break;

  case func_callK:
    // std::cout  << "func\n";
    newE->val.func_callE = copyFunc(e->val.func_callE);
    break;

  case dotK:
    // std::cout  << "dot\n";
    newE->val.dotE.left = (char *) malloc(strlen(e->val.dotE.left));
    strcpy(newE->val.dotE.left, e->val.dotE.left);

    newE->val.dotE.right = (char *) malloc(strlen(e->val.dotE.right));
    strcpy(newE->val.dotE.right, e->val.dotE.right);
    break;

  case eqCheckK:
    // std::cout  << "eq\n";
    newE->val.eqCheckE.left = copyExp(e->val.eqCheckE.left);
    newE->val.eqCheckE.right = copyExp(e->val.eqCheckE.right);
    break;

  case unknownK:
    // std::cout  << "unknown\n";
    break;

  case regexK:
    newE->val.regexE = (char *) malloc(strlen(e->val.regexE));
    strcpy(newE->val.regexE, e->val.regexE);
    break;



    



  default:
    // std::cout  << "mystery?\n";
    break;
  }
  // std::cout  << "leaving\n";
  
  return newE;
}

FUNC *copyFunc(FUNC *f) {
  FUNC *newF = new FUNC;
  newF->id = (char *) malloc(strlen(f->id));
  strcpy(newF->id, f->id);

  newF->args = copyArg_list(f->args);
  return newF;
}

FUNC_DECL *copyFunc_decl(FUNC_DECL *f) {
  FUNC_DECL *newF = new FUNC_DECL;

  newF->tp = (char *)malloc(strlen(f->tp));
  strcpy(newF->tp, f->tp);

  newF->id = (char *)malloc(strlen(f->id));
  strcpy(newF->id, f->id);
  
  newF->args = copyArg_list(f->args);
  newF->program = copyProgram(f->program);
  return newF;
}




// TODO - causes seg fault in pretty printer
ARG_LIST *copyArg_list(ARG_LIST *l) {
  ARG_LIST *newL = new ARG_LIST;
  newL->kind = l->kind;
  newL->decl_kind = l->decl_kind;
  switch (l->kind) {
  case emptyArgK:
    break;
    
    
  case firstK:
    switch (l->decl_kind) {
    case declK:
      newL->val.firstDeclE.tp = (char *)malloc(strlen(l->val.firstDeclE.tp));
      strcpy(newL->val.firstDeclE.tp, l->val.firstDeclE.tp);
      newL->val.firstDeclE.id = (char *)malloc(strlen(l->val.firstDeclE.id));
      strcpy(newL->val.firstDeclE.id, l->val.firstDeclE.id);
      newL->val.firstDeclE.rest = copyArg_list(l->val.firstDeclE.rest);
      break;
    case nonDeclK:
      newL->val.firstE.id = copyExp(l->val.firstE.id);
      newL->val.firstE.rest = copyArg_list(l->val.firstE.rest);
      break;
    }
    break;
  case restK:
    switch (l->decl_kind) {
    case declK:
      newL->val.restDeclE.old = copyArg_list(l->val.restDeclE.old);

      newL->val.restDeclE.tp =(char *) malloc(strlen(l->val.restDeclE.tp));
      strcpy(newL->val.restDeclE.tp, l->val.restDeclE.tp);

      newL->val.restDeclE.id =(char *) malloc(strlen(l->val.restDeclE.id));
      strcpy(newL->val.restDeclE.id, l->val.restDeclE.id);


      
      break;
    case nonDeclK:
      newL->val.restE.old = copyArg_list(l->val.restE.old);
      newL->val.restE.id = copyExp(l->val.restE.id);
      break;
      
    }
    break;
  }  


  return newL;
}
