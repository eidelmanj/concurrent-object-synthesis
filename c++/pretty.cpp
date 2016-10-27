
#include "tree.h"

#include <iostream>
#include <string>
#include <sstream>
#include "external_enums.hh"
#include "pretty.hh"




void prettyPROGRAM(PROGRAM *p) {
  // std::cout << "printing program\n";
  switch(p->kind) {
  case emptyK:
    break;
  case fullK:
    prettyPROGRAM(p->prgrm);
    // std::cout << "printing statement\n";
    prettySTMT(p->stmt);
    break;
    

  
  }
}

void prettySTMT(STMT *s) {
  switch(s->kind) {
  case expK:
    prettyEXP(s->val.expE.e);
    if (s->val.expE.e->kind!=regexK)
      std::cout << ";\n";
    break;
  case func_declK:
    prettyFUNC_DECL(s->val.func_declE.f);
    break;
  case funcK:
    prettyFUNC(s->val.funcE.f);
  case decl_noAssignK:
    std::cout << s->val.decl_noAssignE.tp << " " << s->val.decl_noAssignE.id << ";\n";
    break;
  case decl_assignK:
    std::cout << s->val.decl_assignE.tp << " " << s->val.decl_assignE.id << " = ";

    prettyEXP(s->val.decl_assignE.assignment);

    std::cout << ";\n";
    break;
  case assignK:
    std::cout << s->val.assignE.id << " = ";
    prettyEXP(s->val.assignE.assignment);
    std::cout << ";\n";
    break;
    
  case ifK:
    std::cout << "if (";
    prettyEXP(s->val.ifE.e);
    std::cout << ") {\n";
    prettyPROGRAM(s->val.ifE.p1);
    std::cout << "}\n";
    break;

  case ifElseK:
    std::cout << "if (";
    prettyEXP(s->val.ifElseE.e);
    std::cout << ") {\n";
    prettyPROGRAM(s->val.ifElseE.p1);
    std::cout << "}";
    std::cout << "else {\n";
    prettyPROGRAM(s->val.ifElseE.p2);
    std::cout << "}\n";
    break;


  case assertK:
    std::cout << "assert ";
    prettyEXP(s->val.assertE);
    std::cout <<";\n";
    break;
  case returnK:
    std::cout << "return ";
    prettyEXP(s->val.returnE);
    std::cout << ";\n";
    break;

  case RWfunc_declK:

    // std::cout << "[";    
    // prettyARG_LIST(s->val.RWfunc_declE.shared);

    // std::cout << "]";
    // std::cout << "[";

    // prettyARG_LIST(s->val.RWfunc_declE.reads);
    // std::cout << "]";
    // std::cout << "[";
    // prettyARG_LIST(s->val.RWfunc_declE.writes);
    // std::cout << "]\n";

    prettyFUNC_DECL(s->val.RWfunc_declE.f);
    break;

  case repeatK:
    std::cout << "loop(";
    prettyEXP(s->val.repeatE.e);
    std::cout << ") {\n";
    prettyPROGRAM(s->val.repeatE.p1);
    std::cout << "}\n";
    break;

  case reorderK:
    std::cout << "reorder {\n";
    prettySTMT(s->val.reorderE.s1);
    prettySTMT(s->val.reorderE.s2);
    std::cout << "}\n";
    break;
      

  default:
    std::cout << "saw something\n";
    break;
  }
}


void prettyARG_LIST(ARG_LIST *l) {

  switch (l->kind) {
  case emptyArgK:

    break;

    
  case firstK:

    switch (l->decl_kind) {
    case declK:
      std::cout << l->val.firstDeclE.tp << " " << l->val.firstDeclE.id;
      prettyARG_LIST(l->val.firstDeclE.rest);
      break;
    case nonDeclK:
      prettyEXP(l->val.firstE.id);
      prettyARG_LIST(l->val.firstE.rest);
      break;
    }
    break;
  case restK:
    switch (l->decl_kind) {
    case declK:
      prettyARG_LIST(l->val.restDeclE.old);

      std::cout << ", "<< l->val.restDeclE.tp << " " << l->val.restDeclE.id;
      break;
    case nonDeclK:

      prettyARG_LIST(l->val.restE.old);
      std::cout << ", ";
      prettyEXP(l->val.restE.id);

      
      break;
    }
    break;

  }
}

void prettyEXP(EXP *e) {

  switch(e->kind) {
  case intconstK:
    std::cout << e->val.intconstE;
    break;
  case idK:
    std::cout << e->val.idE;
    break;
  case timesK:
    std::cout << "(";
    prettyEXP(e->val.timesE.left);
    std::cout << "*";
    prettyEXP(e->val.timesE.right);
    std::cout << ")";
    break;
  case divK:
    std::cout << "(";
    prettyEXP(e->val.divE.left);
    std::cout << "\\";
    prettyEXP(e->val.divE.right);
    std::cout << ")";
    break;
  case plusK:
    std::cout << "(";
    prettyEXP(e->val.plusE.left);
    std::cout << "+";
    prettyEXP(e->val.plusE.right);
    std::cout << ")";
    break;
  case minusK:
    std::cout << "(";
    prettyEXP(e->val.minusE.left);
    std::cout << "-";
    prettyEXP(e->val.minusE.right);
    std::cout << ")";
    break;
  case eqCheckK:
    std::cout << "(";
    prettyEXP (e->val.eqCheckE.left);
    std::cout << "==";
    prettyEXP (e->val.eqCheckE.right);
    std::cout << ")";
    break;
  case unknownK:
    std::cout << "??";
    break;
  case func_callK:
    prettyFUNC(e->val.func_callE);
    break;
  case dotK:
    std::cout << e->val.dotE.left;
    std::cout << ".";
    std::cout << e->val.dotE.right;
    break;
  case regexK:
    // std::cout << "this is a line\n";
    std::cout << e->val.regexE;
    break;
  default:
    break;
  }
}



void prettyFUNC_DECL(FUNC_DECL *f) {
  // std::cout << "TRYING FUNC\n";
  std::cout << f->tp << " " << f->id << "(";
  // std::cout << "printed strings\n";
  prettyARG_LIST(f->args);
  // std::cout << "printed list\n";
  std::cout << ") {\n";
  prettyPROGRAM(f->program);
  std::cout << "\n}\n";
}

void prettyFUNC(FUNC *f) {
  std::cout << f->id << "(";
  prettyARG_LIST(f->args);
  std::cout << ")";
}



std::string EXPString(EXP *e) {
  std::string s1, s2, str;
  std::ostringstream stm, stm2 ;
  switch(e->kind) {
  case intconstK:

    stm <<  e->val.intconstE;
    return stm.str();
    break;
  case idK:

    stm2 << e->val.idE;
    return stm2.str();

    break;
  case timesK:

    s1 = EXPString(e->val.timesE.left);
    s2 = EXPString(e->val.timesE.right);
    str = "(";
    str.append(s1);
    str.append("*");
    str.append(s2);
    str.append(")");
    return str;
      

    break;
  case divK:
    s1 = EXPString(e->val.divE.left);
    s2 = EXPString(e->val.divE.right);
    str = "(";
    str.append(s1);
    str.append("\\");
    str.append(s2);
    str.append(")");
    return str;
    
    
    break;
  case plusK:
    s1 = EXPString(e->val.plusE.left);
    s2 = EXPString(e->val.plusE.right);
    str = "(";
    str.append(s1);
    str.append("+");
    str.append(s2);
    str.append(")");
    return str;
    break;
  case minusK:
     s1 = EXPString(e->val.minusE.left);
     s2 = EXPString(e->val.minusE.right);
     str = "(";
    str.append(s1);
    str.append("-");
    str.append(s2);
    str.append(")");
    return str;
    break;
  case eqCheckK:
     s1 = EXPString(e->val.eqCheckE.left);
     s2 = EXPString(e->val.eqCheckE.right);
     str = "(";
    str.append(s1);
    str.append("==");
    str.append(s2);
    str.append(")");
    return str;
    break;
  case unknownK:
     str = ("??");
    return str;
    break;
  case func_callK:
    // prettyFUNC(e->val.func_callE);
     str = "";
    return str;
    break;
  case dotK:
     str = "";
    return str;
    // std::cout << e->val.dotE.left;
    // std::cout << ".";
    // std::cout << e->val.dotE.right;
  default:
    break;
  }
  
}
