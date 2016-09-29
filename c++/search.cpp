#include "tree.h"
#include "tree_interface.hh"
#include "search.hh"
#include "pretty.hh"
#include <cassert>
#include <cstring>
#include <stdlib.h>
#include <stdio.h>
#include "external_enums.hh"
#include <vector>
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <algorithm>
#include <unistd.h>

using namespace std;






// vector< vector<string> >searcher::sequential_string_search(vector<string> stmtList, int depth, int curVarId) {
//   vector<PROGRAM*> retPrgrmVec;
//   vector<PROGRAM*> recursiveRet;

//   // If we've reached the max depth we plan to explore, return an empty program
//   if (depth==0) {
//     return retPrgrmVec;
//   }




//   int thisLevelVarId;
//   for (int transformation = 0; transformation<2; transformation++) {
//     thisLevelVarId = curVarId;

//     if (transformation==0)
//       thisLevelVarId = addReadStrings(stmtList, curVarId);
//     else if (transformation==1)
//       thisLevelVarId = addWriteStrings(stmtList, curVarId);

//     if () {
//       retPrgrmVec.push_back(retPrgrm);


//       recursiveRet = sequential_search(retPrgrm, depth-1, t, thisLevelVarId);
//       retPrgrmVec.insert(retPrgrmVec.end(), recursiveRet.begin(), recursiveRet.end());
//     }
    
//   }

  

//   return retPrgrmVec;



// }





/************* Public Methods **********************/

//Constructor
searcher::searcher(vector< pair<string, string> > shared_in, vector<string> reads_in, vector<string> writes_in, string funcReturnType_in) {
    shared = shared_in;
    reads = reads_in;
    writes = writes_in;
    varId = 0;
    returnType.insert(make_pair("get", "Node"));
    returnType.insert(make_pair("remove", "Node"));
    funcReturnType = funcReturnType_in;
    funcArity.insert(make_pair("get", 2));
    funcArity.insert(make_pair("remove", 2));
    funcArity.insert(make_pair("add", 2));

    funcArity.insert(make_pair("this", 3));


    vector< pair<string, string> > tmpVec;
    tmpVec.push_back(make_pair("val", "int"));
    tmpVec.push_back(make_pair("next", "node"));
		     

}




// STMT *searcher::transform_at_depthStmt(STMT *s, transform_choice transform, int depth) {

//   PROGRAM *transformedP;
//   PROGRAM *recursiveProgram;



//   switch(s->kind) {
//   case ifK:

//     if (depth>1) {
//       recursiveProgram = transform_at_depth(s->val.ifE.p1, transform, depth-1);
//       freeProgram(s->val.ifE.p1);
//       s->val.ifE.p1 = recursiveProgram;
//     }

    
//     transformedP = perform_transformation(s->val.ifE.p1, transform);
//     freeProgram(s->val.ifE.p1);
//     s->val.ifE.p1 = transformedP;


    
//     break;
//   case repeatK:
//     if (depth>1) {
//       recursiveProgram = transform_at_depth(s->val.repeatE.p1, transform, depth-1);
//       freeProgram(s->val.repeatE.p1);
//       s->val.repeatE.p1 = recursiveProgram;
//     }

    
//     transformedP = perform_transformation(s->val.repeatE.p1, transform);
//     freeProgram(s->val.repeatE.p1);
//     s->val.repeatE.p1 = transformedP;

    
//     break;
//   case ifElseK:
//     if (depth>1) {
//       recursiveProgram = transform_at_depth(s->val.ifElseE.p1, transform, depth-1);
//       freeProgram(s->val.ifElseE.p1);
//       s->val.ifElseE.p1 = recursiveProgram;
//     }

    
//     transformedP = perform_transformation(s->val.ifElseE.p1, transform);
//     freeProgram(s->val.ifElseE.p1);
//     s->val.ifElseE.p1 = transformedP;

//     if (depth>1) {
//       recursiveProgram = transform_at_depth(s->val.ifElseE.p2, transform, depth-1);
//       freeProgram(s->val.ifElseE.p2);
//       s->val.ifElseE.p2 = recursiveProgram;
//     }

    
//     transformedP = perform_transformation(s->val.ifElseE.p2, transform);
//     freeProgram(s->val.ifElseE.p2);
//     s->val.ifElseE.p2 = transformedP;


//     break;
//   default:
//     break;
//   }

//   return s;

// }

// PROGRAM *searcher::transform_at_depth(PROGRAM *p, transform_choice transform, int depth) {

  
//   PROGRAM *newP = copyProgram(p);
//   PROGRAM *innerP = newP;

//   PROGRAM *nextP;

//   if (newP->kind==fullK && newP->stmt->kind == RWfunc_declK)
//     innerP = newP->stmt->val.RWfunc_declE.f->program;


//   switch (innerP->kind) {
//   case emptyK:
//     break;

//   case fullK:

//     transform_at_depthStmt(innerP->stmt, transform, depth);


//     nextP = transform_at_depth(innerP->prgrm, transform, depth);
//     freeProgram(innerP->prgrm);
//     innerP->prgrm = nextP;

//     break;
//   }

//   return newP;
  
// }


int searcher::freshVar() {
  this->varId++;
  return this->varId;
}

string searcher::freshVarId() {
  string freshVarId = "z" + static_cast<ostringstream*>( &(ostringstream() << freshVar()) )->str();
  return freshVarId;
}

string searcher::getVarId(int id) {
  string varId = "z" + static_cast<ostringstream*>( &(ostringstream() << id) )->str();
  return varId;
}


//Perform a single transformation 
pair<PROGRAM*, int> searcher::perform_transformation(PROGRAM *p, transform_choice transform, int curVarId) {
  PROGRAM *newP = copyProgram(p);
  PROGRAM *innerP = newP;

  STMT *declareRet = new STMT;
  STMT *returnStmt = new STMT;

  EXP *retExp;




  // We only want to perform transformations on the inside of the function, not on the function declaration
  if (newP->kind==fullK && newP->stmt->kind == RWfunc_declK) {
    innerP = newP->stmt->val.RWfunc_declE.f->program;

    // // This code ensures that the sketch returns the correct data type
    // if (is_empty(innerP)) {
    //   declareRet->kind = decl_noAssignK;
    //   declareRet->val.decl_noAssignE.tp = (char *) malloc(strlen(newP->stmt->val.RWfunc_declE.f->tp));
    //   strcpy(declareRet->val.decl_noAssignE.tp, newP->stmt->val.RWfunc_declE.f->tp);

    //   declareRet->val.decl_noAssignE.id = (char *) malloc(500);
    //   sprintf(declareRet->val.decl_noAssignE.id, "z%d", freshVar());


    //   returnStmt->kind = returnK;

    //   retExp = new EXP;

    //   retExp->kind = idK;
    //   retExp->val.idE = (char *) malloc(strlen(declareRet->val.decl_noAssignE.id));
    //   strcpy(retExp->val.idE, declareRet->val.decl_noAssignE.id);

    //   returnStmt->val.returnE = retExp;

    //   innerP->prgrm = new PROGRAM;
    //   innerP->stmt = returnStmt;
    //   innerP->kind = fullK;


    //   innerP->prgrm->kind = fullK;
    //   innerP->prgrm->stmt = declareRet;
    //   innerP->prgrm->prgrm = new PROGRAM;
    //   innerP->prgrm->prgrm->kind = emptyK;
      

      
      
    // }
  }



  
  switch (transform) {
  case t1:
    // cout << "Adding read statments\n";
    curVarId = addReadStmt(innerP, curVarId);
    break;
  case t2:
    // cout << "Adding write statements\n";
    curVarId = addWriteStmt(innerP, curVarId);
    break;
  default:
    // cout << "Returning empty program\n";
    freeProgram(newP);
    newP = new PROGRAM;
    newP->kind = emptyK;
    break;
  // case t1:
  //   addGuards(innerP);
  //   break;
  // case t2:
  //   addRepeats(innerP);
  //   break;

  // case t3:
  //   addReorder(innerP);
  //   break;

  // case t4:
  //   addReadStmt(innerP);
  //   break;

  // case t5:
  //   addWriteStmt(innerP);
  }

  return make_pair(newP, curVarId);

  
}


int searcher::get_arity(string whichFunc) {
  return funcArity.at(whichFunc);
}

vector< pair<PROGRAM*, int> > searcher::sequential_search(PROGRAM *p, int depth,
					     transform_choice noRepeat = tNone, int curVarId=0) {
					     
  vector< pair<PROGRAM*, int> > retPrgrmVec;
  vector< pair<PROGRAM*, int> > recursiveRet;
  pair<PROGRAM*, int> retPair;
  PROGRAM *retPrgrm;
  // If we've reached the max depth we plan to explore, return an empty program
  if (depth==0) {
    retPrgrm = new PROGRAM;
    retPrgrm->kind = emptyK;
    retPrgrmVec.push_back(make_pair(retPrgrm, curVarId));
    return retPrgrmVec;
  }




  int thisLevelVarId;
  for (int transformation = t1; transformation!=t5; transformation++) {
    thisLevelVarId = curVarId;
    transform_choice t = static_cast<transform_choice>(transformation);
    if (t==noRepeat)
      continue;
    


    // cout << "Performing transformation - at level: " << depth << "\n";
    retPair = perform_transformation(p, t, thisLevelVarId);
    retPrgrm = retPair.first;
    thisLevelVarId = retPair.second;

    if (!is_empty(retPrgrm)) {
      retPrgrmVec.push_back(retPair);


      recursiveRet = sequential_search(retPrgrm, depth-1, t, thisLevelVarId);
      retPrgrmVec.insert(retPrgrmVec.end(), recursiveRet.begin(), recursiveRet.end());
    }
    
  }

  

  return retPrgrmVec;
}

int searcher::is_empty_stmt(STMT *s) {
  switch(s->kind) {
  case RWfunc_declK:
    return is_empty(s->val.RWfunc_declE.f->program);
    break;
  default:
    return 0;
    break;
  }
}

int searcher::is_empty(PROGRAM *p) {
  switch (p->kind) {
  case emptyK:
    return 1;
    break;

  case fullK:
    return is_empty_stmt(p->stmt);
  default:
    return 0;
    break;
  }
  return 0;
}



bool replace(std::string& str, const std::string& from, const std::string& to) {
  size_t start_pos = str.find(from);
  if(start_pos == std::string::npos)
    return false;
  str.replace(start_pos, from.length(), to);
  return true;
}


int searcher::runSketch(PROGRAM *p, int i) {
  stringstream ss;
  ss << i;
  string str_i = ss.str();

  string tmpFileName = ("test/out" + str_i + ".txt");
  ofstream out(tmpFileName.c_str());
  streambuf *coutbuf = cout.rdbuf(); //save old buf
  cout.rdbuf(out.rdbuf()); //redirect std::cout to out.txt!

  prettyPROGRAM(p);
  cout.rdbuf(coutbuf);
  out.close();


  string line, sketchText = ("");
  ifstream sketchFile (tmpFileName.c_str());
  if (sketchFile.is_open()) {

    while ( getline (sketchFile,line) ){

      sketchText = sketchText+line + ("\n");

    }
    sketchFile.close();
  }
  else {

  }

  string backgroundText = ("");
  ifstream backgroundFile("background.sk");
  if (backgroundFile.is_open()) {

    while ( getline (backgroundFile,line) ){

      backgroundText = backgroundText+line + ("\n");

    }
    backgroundFile.close();
  }


  string originalText = ("");
  ifstream originalFile("test/original.txt");
  if (originalFile.is_open()) {

    while ( getline (originalFile,line) ){

      originalText = originalText+line + ("\n");

    }
    originalFile.close();
  }


  
  replace(backgroundText, "SKETCH", sketchText);
  replace(backgroundText, "ORIGINAL", originalText);



  string completeFileName = ("outComplete" + str_i + ".txt");
  ofstream outComplete(completeFileName.c_str());

  outComplete << backgroundText;

  outComplete.close();


  string outputLogFileName = ("outLog" + str_i + ".txt");

  system(("./run_test.sh " + completeFileName + " > " + outputLogFileName).c_str());


  

  string logText = ("");
  ifstream logFile (outputLogFileName.c_str());
  if (logFile.is_open()) {
    
    while ( getline (logFile,line) ){

      logText = logText+line + ("\n");

    }
    logFile.close();
  }
  else {

  }


  if (logText.find("BUILD SUCCESS") != string::npos) {
    return SUCCESS;
  }
  else {
    return FAIL;
  }

  




  


  


  
}


/************* Private Methods *************************/

void searcher::addRepeats(PROGRAM *p) {
  switch(p->kind) {
  case emptyK:
    break;
  case fullK:
    if (p->stmt->kind==decl_noAssignK || p->stmt->kind==decl_assignK)
      break;

    STMT *newS = new STMT;
    newS->kind = repeatK;

    EXP *innerE = new EXP;
    innerE->kind = unknownK;
    
    
    PROGRAM *emptyInnerP = new PROGRAM;
    emptyInnerP->kind = emptyK;

    PROGRAM *innerP = new PROGRAM;
    innerP->kind = fullK;
    innerP->stmt = p->stmt;
    innerP->prgrm = emptyInnerP;

    newS->val.repeatE.e = innerE;
    newS->val.repeatE.p1 = innerP;

    p->stmt = newS;

    // addRepeats(p->prgrm);

      
    
    break;
  }
}



void searcher::addGuards(PROGRAM *p) {
  switch(p->kind) {
  case emptyK:
    break;
  case fullK:
    if (p->stmt->kind==decl_noAssignK || p->stmt->kind==decl_assignK)
      break;
    
    STMT *newS = new STMT;
    newS->kind = ifK;

    EXP *innerE = new EXP;
    innerE->kind = unknownK;
    
    
    PROGRAM *emptyInnerP = new PROGRAM;
    emptyInnerP->kind = emptyK;

    PROGRAM *innerP = new PROGRAM;
    innerP->kind = fullK;

    innerP->stmt = p->stmt;
    innerP->prgrm = emptyInnerP;

    newS->val.ifE.e = innerE;
    newS->val.ifE.p1 = innerP;

    p->stmt = newS;

    // addGuards(p->prgrm);

      
    
    break;
  }
}






void searcher::addReorder(PROGRAM *p) {
  // cout << "adding reorder...\n";
  switch(p->kind) {
  case emptyK:
    break;
  case fullK:
    if (p->stmt->kind==decl_noAssignK || p->stmt->kind==decl_assignK)
      break;

    STMT *newS = new STMT;

    newS->kind = reorderK;

    
    STMT *nextS = getNextStmt(p->prgrm);
    if (nextS->kind == nullK)
      break;
    else {
      // cout << "ACTUAL REORDER\n";
      newS->val.reorderE.s1 = p->stmt;
      newS->val.reorderE.s2 = nextS;
      p->stmt = newS;
      p->prgrm = p->prgrm->prgrm;
      // prettyPROGRAM(p);
      addReorder(p->prgrm);
    }


    break;
  }
}


vector<string> searcher::generateCall(string varId, string methodName, string callerName, int curVarId) {
  int n, r;

  n = curVarId;
  r = funcArity.at(methodName) -1 ;
  
  vector<bool> v(n);
  fill(v.begin() + n - r, v.end(), true);

  vector< vector<int> > allTuples;

  
  
  do {
    vector<int> tmp;
    for (int i = 0; i < n; ++i) {
      if (v[i]) {
	tmp.push_back(i);
      }
    }
    allTuples.push_back(tmp);

  } while (std::next_permutation(v.begin(), v.end()));

  vector<string> retVec;

  for (int i=0; i<allTuples.size(); i++) {
    retVec.push_back("");
    if (varId!="")
      retVec[i] = varId + "=" + methodName + "(" + callerName;
    else
      retVec[i] = methodName + "(" + callerName;

    for (int j=0; j<allTuples[i].size(); j++) {
      retVec[i] = retVec[i] + "," +  getVarId(allTuples[i][j]);
    }
    retVec[i] = retVec[i] + ")";
  }


  


  return retVec;
}

int searcher::addReadStmt(PROGRAM *p, int curVarId) {
  PROGRAM *newP;
  STMT *newS;
  EXP *newE;
    
  string freshVarIdStr;
  curVarId++;
  vector<string> funcCallVec;
  switch(p->kind) {
  case emptyK: {
    newP = new PROGRAM;
    newS = new STMT;
    newE = new EXP;

    

    
    newE->kind = regexK;

    string regexStr = "";
    for (int i = get_arity("this"); i< curVarId; i++) {
      regexStr += "if(??) {" + getVarId(i) + " = {|";	  
      for(vector<string>::iterator it = reads.begin(); it != reads.end(); ++it) {
	for (vector< pair<string, string> >::iterator jt = shared.begin(); jt != shared.end(); ++jt) {

	  funcCallVec = generateCall("", *it, (*jt).second, curVarId);
	  // We try assigning the result of this read to each existing variable
	  // For variables that have no type yet, assign them a type

	  if (varType.count(i)<1) {
	    

	    for (vector<string>::iterator ct = funcCallVec.begin(); ct != funcCallVec.end(); ++ct) {
	      regexStr = regexStr + *ct + " |";
	    }



				       
	    // regexStr =  regexStr + generateCall(getVarId(i), *it, (*jt).second)[0]; // getVarId(i) + "=" +  *it + "(" + (*jt).second + ")";
	    // regexStr = regexStr + " | ";
	    varType.insert(make_pair(i, returnType.at(*it)));
	  }
	  // For variables that have a type, if the type matches the read we're doing
	  // then perform the assignment
	  else if (varType.at(i) == returnType.at(*it)) {
	    for (vector<string>::iterator ct = funcCallVec.begin(); ct != funcCallVec.end(); ++ct) {
	      regexStr = regexStr + *ct + " |";
	    }

	    // regexStr =  regexStr + generateCall(getVarId(i), *it, (*jt).second)[0]; // getVarId(i) + "=" +  *it + "(" + (*jt).second + ")";
	    // regexStr = regexStr + " | ";
	  }



	}
      }
      regexStr += "}; }\n";
    }

    // TODO: Need a better way to do this
    // We need to create fresh variables to save the results of our read statement
    // for(vector<string>::iterator it = reads.begin(); it != reads.end(); ++it) {
    //   for (vector< pair<string, string> >::iterator jt = shared.begin(); jt != shared.end(); ++jt) {
    // 	curVarId++;
    // 	freshVar();
    //   }
    // }





    

    const char *tmpStr = regexStr.c_str();
    newE->val.regexE = (char *) malloc(strlen(tmpStr));
    strcpy(newE->val.regexE, tmpStr);

    newS->kind = expK;
    newS->val.expE.e = newE;

    newP->kind = emptyK;


    // newP->kind = fullK;
    // newP->stmt = newS;
    // newP->prgrm = p->prgrm;
    p->prgrm = newP;
    p->stmt = newS;
    p->kind = fullK;

    // addGuards(p);
    // addRepeats(p);

    break;
  }

  case fullK: {
    newP = new PROGRAM;
    newS = new STMT;
    newE = new EXP;



    // curVarId = addReadStmt(p->prgrm, curVarId);

        newE->kind = regexK;

    string regexStr = "";
    for (int i = get_arity("this"); i< curVarId; i++) {
      regexStr += "if(??) {" + getVarId(i) + " = {|";	  
      for(vector<string>::iterator it = reads.begin(); it != reads.end(); ++it) {
	for (vector< pair<string, string> >::iterator jt = shared.begin(); jt != shared.end(); ++jt) {

	  funcCallVec = generateCall("", *it, (*jt).second, curVarId);
	  // We try assigning the result of this read to each existing variable
	  // For variables that have no type yet, assign them a type

	  if (varType.count(i)<1) {
	    

	    for (vector<string>::iterator ct = funcCallVec.begin(); ct != funcCallVec.end(); ++ct) {
	      regexStr = regexStr + *ct + " |";
	    }



				       
	    // regexStr =  regexStr + generateCall(getVarId(i), *it, (*jt).second)[0]; // getVarId(i) + "=" +  *it + "(" + (*jt).second + ")";
	    // regexStr = regexStr + " | ";
	    varType.insert(make_pair(i, returnType.at(*it)));
	  }
	  // For variables that have a type, if the type matches the read we're doing
	  // then perform the assignment
	  else if (varType.at(i) == returnType.at(*it)) {
	    for (vector<string>::iterator ct = funcCallVec.begin(); ct != funcCallVec.end(); ++ct) {
	      regexStr = regexStr + *ct + " |";
	    }

	    // regexStr =  regexStr + generateCall(getVarId(i), *it, (*jt).second)[0]; // getVarId(i) + "=" +  *it + "(" + (*jt).second + ")";
	    // regexStr = regexStr + " | ";
	  }



	}
      }
      regexStr += "}; }\n";
    }




    const char *tmpStr = regexStr.c_str();
    newE->val.regexE = (char *) malloc(strlen(tmpStr));
    strcpy(newE->val.regexE, tmpStr);

    newS->kind = expK;
    newS->val.expE.e = newE;

    newP->kind = fullK;
    newP->stmt = newS;
    newP->prgrm = p->prgrm;
    p->prgrm = newP;


    addGuards(newP);
    addRepeats(newP);
    break;
  }
  }
  return curVarId;
}




int searcher::addStructAccess(PROGRAM *p, int curVarId) {
  PROGRAM *newP = new PROGRAM;
  STMT *newS = new STMT;
  EXP *newE = new EXP;

  switch(p->kind) {
  case fullK:
    curVarId = addStructAccess(p->prgrm, curVarId);
    break;
  default:
    break;
  }


  string regexStr = "{|";
  for (int i=0; i<curVarId; i++) {
    for (int j=0; j<curVarId; j++) {
      
    }
  }

  
    
  
  return curVarId;
}

int searcher::addWriteStmt(PROGRAM *p, int curVarId) {
  switch(p->kind) {
  case emptyK:
    break;

  case fullK:
    PROGRAM *newP = new PROGRAM;
    STMT *newS = new STMT;
    EXP *newE = new EXP;

    // addWriteStmt(p->prgrm, curVarId);



    newE->kind = regexK;
    // string regexStr = "{|";
    string regexStr = ("");

    for(vector<string>::iterator it = writes.begin(); it != writes.end(); ++it) {
      for (vector< pair<string, string> >::iterator jt = shared.begin(); jt != shared.end(); ++jt) {
	vector<string> allCalls = generateCall("", *it,  (*jt).second, curVarId);

	for (vector<string>::iterator ct = allCalls.begin(); ct != allCalls.end(); ++ct) {
	  regexStr = regexStr + "if (??) {" + *ct + ";}\n";
	}
	
	// regexStr = regexStr + "loop(??) {if (??) {";
	// regexStr = regexStr + *it + "(" + (*jt).second + ");";
	// regexStr = regexStr +"}}\n";
	// regexStr = regexStr + " | ";
      }
      
    }

    regexStr += " ";

    newP = copyProgram(p);

    
    const char *tmpStr = regexStr.c_str();
    newE->val.regexE = (char *) malloc(strlen(tmpStr));
    strcpy(newE->val.regexE, tmpStr);

    newS->kind = expK;
    newS->val.expE.e = newE;

    p->kind = fullK;
    p->stmt = newS;

    freeProgram(p->prgrm);
    p->prgrm = newP;
    // if (p->prgrm->kind==emptyK){
    //   newP->prgrm = new PROGRAM;
    //   newP->prgrm->kind = emptyK;
    // }
    // else
    //   newP->prgrm = p->prgrm->prgrm;






    break;
  }
  return curVarId;
}



void searcher::addReturn(PROGRAM *p, int numVars) {
  PROGRAM *innerP;
  if (p->kind==fullK && p->stmt->kind == RWfunc_declK) {
    innerP = p->stmt->val.RWfunc_declE.f->program;
  }

  if (innerP->kind!=emptyK && innerP->kind!=fullK) {
    return;
  }

  EXP *newE = new EXP;
  string regexStr = "{|";
  PROGRAM *newP = copyProgram(innerP);
  



  for (int i=0; i<numVars; i++) {
    regexStr += getVarId(i) + " |";
  }

  regexStr+="}";

  newE->kind = regexK;
  newE->val.regexE = (char *) malloc(regexStr.size());
  strcpy(newE->val.regexE, regexStr.c_str());
  
  
  STMT *newRet = new STMT;
  newRet->kind = returnK;
  newRet->val.returnE = newE;


  innerP->kind = fullK;
  innerP->stmt = newRet;
  innerP->prgrm = newP;
}

void searcher::addInits(PROGRAM *p, int numVars, vector<string> args) {
  // cout << "adding inits with numVar=" << numVars << "\n";




  
  PROGRAM *tmpP = new PROGRAM;
  tmpP->kind = emptyK;
  
  PROGRAM *newP;
  STMT *newS;
  EXP *newE;

  PROGRAM *innerP;

  if (p->kind==fullK && p->stmt->kind == RWfunc_declK) {
    innerP = p->stmt->val.RWfunc_declE.f->program;
  }

  if (innerP->kind!=emptyK && innerP->kind!=fullK) {
    return;
  }

  for (int i=0; i<numVars; i++) {
    // cout << "creating new init program\n";

    if (find(args.begin(), args.end(), getVarId(i)) != args.end()) {
      continue;
    }

    newE = new EXP;
    newE->kind = intconstK;
    newE->val.intconstE=0;
    
    newS = new STMT;
    newS->kind = decl_assignK;
    newS->val.decl_assignE.tp = (char *) malloc(strlen("int"));
    strcpy(newS->val.decl_assignE.tp, "int");

    newS->val.decl_assignE.id = (char *) malloc(getVarId(i).size());
    strcpy(newS->val.decl_assignE.id, getVarId(i).c_str());

    newS->val.decl_assignE.assignment = newE;

    
    
    
    newP = new PROGRAM;
    newP->kind = fullK;
    newP->stmt = newS;
    newP->prgrm = tmpP;
    tmpP = newP;
    
  }

  // cout <<"exited loop\n";
  PROGRAM *cur = innerP;
  if (cur->kind==fullK) {
    // cout << "Not empty\n";
    while (cur->prgrm->kind != emptyK) {
      // cout << "searching...\n";
      cur = cur->prgrm;
    }
  }
  else {
    // cout << "empyt\n";
    cur->kind = fullK;

  }
  // cout << "Found end of prgram\n";
  cur->prgrm = tmpP;
  
  
}

/******************************************************/


STMT *getNextStmt(PROGRAM *p) {
  STMT *tmpS = new STMT;
  switch(p->kind) {
  case emptyK:
    tmpS->kind = nullK;
    return tmpS;
  case fullK:
    return p->stmt;
  }
}



// rw_entry *makeRw_entry() {
//   rw_entry *r;
//   r = new rw_entry;
//   r->id = "hello\n";

//   return r;
// }

// rw_entry **get_reads(STMT *s) {

//   // int d = getDepth(s->val.RWfunc_declE.reads);
//   // rw_entry **entry_list = new rw_entry*[d];
//   // entry_list[0] = makeRw_entry();
//   // return entry_list;
  
// }


vector< pair<string, string> > get_list_decl_entries(ARG_LIST *l) {
  assert(l->decl_kind == declK);
  vector< pair<string, string> > entries;

  ARG_LIST *cur = l;
  
  if(cur->kind != emptyArgK) {
    string tp(cur->val.firstDeclE.tp);
    string id(cur->val.firstDeclE.id);
    // cout << tp << "-" << id << "\n";
    entries.push_back(std::pair<string, string>(tp, id));
    cur = cur->val.firstDeclE.rest;
  }

  while (cur->kind != emptyArgK) {
    string tp(cur->val.restDeclE.tp);
    string id(cur->val.restDeclE.id);
    // cout << tp << "-" << id << "\n";
    entries.push_back(std::pair<string, string>(tp, id));
    cur = cur->val.restDeclE.old;
  }

  return entries;
}


vector<string> get_list_entries(ARG_LIST *l) {
  assert(l->decl_kind == nonDeclK);

  vector<string> reads;

  ARG_LIST *cur = l;

  if (cur->kind != emptyArgK) {
    reads.push_back(EXPString(cur->val.firstE.id));
    cur = cur->val.firstE.rest;
  }
  
  while (cur->kind != emptyArgK) {

    reads.push_back(EXPString(cur->val.restE.id));
    cur = cur->val.restE.old;
  }
  return reads;

  
}



vector<string> get_writes(STMT *s) {
  assert(s->kind == RWfunc_declK);


  ARG_LIST *cur = s->val.RWfunc_declE.writes;
  return get_list_entries(cur);
}



vector<string> get_reads(STMT *s) {
  assert(s->kind == RWfunc_declK);


  ARG_LIST *cur = s->val.RWfunc_declE.reads;
  return get_list_entries(cur);
}


vector< pair<string, string> > get_shared(STMT *s) {
   assert(s->kind == RWfunc_declK);
   ARG_LIST *cur = s->val.RWfunc_declE.shared;
   return get_list_decl_entries(cur);
 }





