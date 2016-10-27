#ifndef SEARCH_H
#define SEARCH_H

#include <vector>
#include <map>
#include <iostream>
#include <string>
#include "tree.h"

using namespace std;

typedef struct rw_entry {
  std::string id;
} rw_entry;


vector<string> get_reads(STMT *s);
vector<string> get_writes(STMT *s);
vector< pair<string, string> > get_shared(STMT *s);
int getDepth(ARG_LIST *l);
STMT *getNextStmt(PROGRAM *p);

// typedef enum {t1, t2, t3, t4, t5, tNone} transform_choice;
typedef enum {t1, tNone} transform_choice;
class searcher{
protected:
  int varId;
  string funcReturnType;
  map<string, string> returnType;
  map<string, int> funcArity;
  map<int, string> varType;
  map<string, vector< pair<string, string> > > structVarMap;

  vector< pair<string, string> > shared;
  vector<string> reads;
  vector<string> writes;
  void addGuards(PROGRAM *p);
  void addRepeats(PROGRAM *p);
  void addReorder(PROGRAM *p);
  int addReadStmt(PROGRAM *p, int curVarId);
  int addWriteStmt(PROGRAM *p, int curVarId);
  int addStructAccess(PROGRAM *p, int curVarId);
  vector<string> generateCall(string varId, string methodName, string callerName, int curVarId);
  int is_empty_stmt(STMT *s);
  int sharedNum();
  int freshVar();
  string freshVarId();
  string getVarId(int id);
  STMT *transform_at_depthStmt(STMT *s, transform_choice transform, int depth);


public:
  const int FAIL = 0;
  const int SUCCESS = 1;

  
  int is_empty(PROGRAM *p);
  searcher(vector< pair<string, string> > shared_in, vector<string> reads_in, vector<string> writes_in, string funcReturnType_in);
  pair<PROGRAM*, int> perform_transformation(PROGRAM *p, transform_choice transform, int curVarId);
  PROGRAM *transform_at_depth(PROGRAM *p, transform_choice transform, int depth);
  int runSketch(PROGRAM *p, int i);
  vector< pair<PROGRAM*, int> > sequential_search(PROGRAM *p, int depth, transform_choice noRepeat, int curVarId);

  void addReturn(PROGRAM *p, int numVars);
  void addAssignment(PROGRAM *p, int numVars);
  void addInits(PROGRAM *p, int numVars, vector<string> args);
  int get_arity(string whichFunc);
};


#endif
