#ifndef OUTCOMPLETE2_H
#define OUTCOMPLETE2_H

#include <cstring>

#include "vops.h"

namespace ANONYMOUS{
  class Node;
  class List;
}
namespace ANONYMOUS{
class Node; 
class List; 
class Node{
  public:
  int  val;
  Node*  next;
  Node(){}
  static Node* create(  int  val_,   Node*  next_);
  ~Node(){
  }
  void operator delete(void* p){ free(p); }
};
class List{
  public:
  Node*  first;
  Node*  last;
  List(){}
  static List* create(  Node*  first_,   Node*  last_);
  ~List(){
  }
  void operator delete(void* p){ free(p); }
};
extern void blah__Wrapper();
extern void blah__WrapperNospec();
extern void blah();
extern void newList(List*& _out);
extern void add(List* l, int newVal);
extern void copy(List* l1, List* l2, int idx, int& _out);
extern void copySKETCH(List* l1, List* l2, int z0, int& _out);
extern void newNode(Node*& _out);
extern void get(List* l, int idx, int& _out);
extern void remove(List* l, int idx, int& _out);
}

#endif
