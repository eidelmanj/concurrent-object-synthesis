#ifndef TREE_HH
#define TREE_HH

PROGRAM *copyProgram(PROGRAM *p);
STMT *copyStmt(STMT *s);
EXP *copyExp(EXP *e);
FUNC *copyFunc(FUNC *f);
FUNC_DECL *copyFunc_decl(FUNC_DECL *f);
ARG_LIST *copyArg_list(ARG_LIST *l);

void freeProgram(PROGRAM *p);
void freeStmt(STMT *s);
void freeExp(EXP *e);
void freeFunc(FUNC *f);
void freeFunc_decl(FUNC_DECL *f);
void freeArg_list(ARG_LIST *l);




#endif
