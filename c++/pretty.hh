#ifndef PRETTY_H
#define PRETTY_H

#include "tree.h"
#include <iostream>
#include <string>

void prettyEXP(EXP *e);
void prettyPROGRAM(PROGRAM *p);
void prettySTMT(STMT *s);
void prettyFUNC_DECL(FUNC_DECL *f);
void prettyFUNC(FUNC *f);
void prettyARG_LIST(ARG_LIST *l);


std::string EXPString(EXP *e);

#endif /* !PRETTY_H */
