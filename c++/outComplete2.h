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
  int  key;
  Node*  next;
  Node(){}
  static Node* create(  int  val_,   int  key_,   Node*  next_);
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
extern void push(List* l, int key, int val, int& _out);
extern void copy(List* l1, List* l2, int key, int& _out);
extern void copySKETCH(List* l1, List* l2, int z0_0, int& _out);
extern void checkList(List* l1, List* l2);
extern void newNode(Node*& _out);
extern void get(List* l, int key, int& _out);
extern void remove(List* l, int key, int& _out);
}

#endif
