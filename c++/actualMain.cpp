#include "tree.h"
#include "pretty.hh"
#include "search.hh"
#include "pretty.hh"
#include "tree_interface.hh"
#include <vector>
#include <stdlib.h>
#include <cassert>
#include <cstring>
#include <fstream>
#include <sstream>
#include <algorithm>

#ifdef __cplusplus
#define EXTERNC extern "C"
#else
#define EXTERNC
#endif




EXTERNC void start(PROGRAM *p);

#undef EXTERNC
// ...


#include <iostream>

using namespace std;

bool is_shared(char *s, vector<string> shared) {
  string x(s);
  
  return find(shared.begin(), shared.end(), x) != shared.end();
  
}


string getVarId(int id) {
  string varId = "z" + static_cast<ostringstream*>( &(ostringstream() << id) )->str();
  return varId;
}


ARG_LIST *renameArg_listVars(ARG_LIST *l, vector<string> shared, int level) {
  
  switch (l->kind) {
  case emptyArgK:

    break;

    
  case firstK:

    switch (l->decl_kind) {
    case declK:
      if (!is_shared(l->val.firstDeclE.id, shared)) {
	  free(l->val.firstDeclE.id);
	  l->val.firstDeclE.id = (char *) malloc(500);
	  strcpy(l->val.firstDeclE.id, getVarId(level).c_str());
	  renameArg_listVars(l->val.firstDeclE.rest, shared,level+1);
	}
      else
	renameArg_listVars(l->val.firstDeclE.rest, shared,level);
      break;
    case nonDeclK:
      // prettyEXP(l->val.firstE.id);
      // renameArg_listVars(l, level+1);
      break;
    }
    break;
  case restK:
    switch (l->decl_kind) {
    case declK:

      if (!is_shared(l->val.restDeclE.id, shared)) {

	renameArg_listVars(l->val.restDeclE.old, shared, level+1);


	free(l->val.restDeclE.id);
	l->val.restDeclE.id = (char *) malloc(500);
	strcpy(l->val.restDeclE.id, getVarId(level).c_str());
      } else
	renameArg_listVars(l->val.restDeclE.old, shared, level);
      // std::cout << ", "<< l->val.restDeclE.tp << " " << l->val.restDeclE.id;
      break;
    case nonDeclK:

      // renameArg_listVars(l, level+1);
      // std::cout << ", ";
      // prettyEXP(l->val.restE.id);

      
      break;
    }
    break;

  }
}


  PROGRAM *createNewEmptySketch(STMT *sOld, vector<string> sharedList) {
  assert(sOld->kind == RWfunc_declK);
  ARG_LIST *args = sOld->val.RWfunc_declE.f->args;
  char *oldTp = sOld->val.RWfunc_declE.f->tp;
  char *oldId = sOld->val.RWfunc_declE.f->id;

  
  PROGRAM *p = new PROGRAM;
  STMT *s = new STMT;
  FUNC_DECL *f = new FUNC_DECL;
  ARG_LIST *l = copyArg_list(args);

  renameArg_listVars(l, sharedList, 0);

  
  PROGRAM *inner = new PROGRAM;

  ARG_LIST *shared = new ARG_LIST;
  shared->kind = emptyArgK;

  ARG_LIST *reads = new ARG_LIST;
  reads->kind = emptyArgK;

  ARG_LIST *writes = new ARG_LIST;
  writes->kind = emptyArgK;

  inner->kind = emptyK;
  
  f->args = l;
  f->tp = (char *) malloc(strlen(oldTp));
  strcpy(f->tp, oldTp);
  f->id = (char *) malloc(strlen(oldId) + 5);
  strcpy(f->id, oldId);
  strcat(f->id, "SKETCH");
  f->program = inner;

  s->kind = RWfunc_declK;
  s->val.RWfunc_declE.f = f;
  s->val.RWfunc_declE.shared = shared;
  s->val.RWfunc_declE.reads = reads;
  s->val.RWfunc_declE.writes = writes;

  p->kind = fullK;
  p->stmt = s;

  PROGRAM *nextP = new PROGRAM;
  nextP->kind = emptyK;
  p->prgrm = nextP;

  
  return p;

}


void start(PROGRAM *p)
{

  // system("rm -f test/original.txt");



  // ofstream outOrig("test/original.txt");
  // streambuf *coutbufOrig = cout.rdbuf(); //save old buf
  // cout.rdbuf(outOrig.rdbuf()); //redirect std::cout to out.txt!

  // prettyPROGRAM(p);
  // outOrig.close();
  // cout.rdbuf(coutbufOrig);





  vector<string> readList = get_reads(p->stmt);
  std::cout << readList[1] << "\n";
  // vector<string> writeList = get_writes(p->stmt);
  vector<string> writeList;
  vector< pair<string, string> > sharedList = get_shared(p->stmt);
  vector<string> sharedIdList;

  for (vector< pair<string, string> >::iterator it=sharedList.begin(); it!=sharedList.end(); ++it) {
    sharedIdList.push_back((*it).second);
  }




  
  searcher space_explorer(sharedList, readList, writeList, "int");


  PROGRAM *emptyProg = createNewEmptySketch(p->stmt, sharedIdList);


  vector<string> args;
  args.push_back("l1");
  args.push_back("l2");
  // args.push_back("z1");
  args.push_back("z0");



  int arity = space_explorer.get_arity("this");
  vector< pair<PROGRAM*, int> > newP = space_explorer.sequential_search(emptyProg, 2, tNone, arity);



  std::cout << "here\n";

  int i =0;
  for(vector< pair<PROGRAM*, int> >::iterator it = newP.begin(); it != newP.end(); ++it) {

    
    space_explorer.addInits((*it).first, (*it).second, args);
    space_explorer.addReturn((*it).first, (*it).second);
    int emptyQ = space_explorer.is_empty((*it).first);

    if (!emptyQ) {
      if (space_explorer.runSketch((*it).first, i)) {
    	cout << "Found a solution!\n";
    	break;
      }
      i++;


    } else {

    }

  }

  

  // prettyPROGRAM(newP);

  // PROGRAM *newP = // searcher::perform_transformation(//copyProgram(p);
  // prettyPROGRAM(newP);
  // freeProgram(newP);

  
 
}
