#include <cstdio>
#include <assert.h>
#include <iostream>
using namespace std;
#include "vops.h"
#include "outComplete2.h"
namespace ANONYMOUS{

Node* Node::create(int  val_, Node*  next_){
  void* temp= malloc( sizeof(Node)  ); 
  Node* rv = new (temp)Node();
  rv->val =  val_;
  rv->next =  next_;
  return rv;
}
List* List::create(Node*  first_, Node*  last_){
  void* temp= malloc( sizeof(List)  ); 
  List* rv = new (temp)List();
  rv->first =  first_;
  rv->last =  last_;
  return rv;
}
void blah__Wrapper() {
  blah();
}
void blah__WrapperNospec() {}
void blah() {
  List*  l1_s5=NULL;
  newList(l1_s5);
  List*  l2_s7=NULL;
  newList(l2_s7);
  List*  l1prime_s9=NULL;
  newList(l1prime_s9);
  List*  l2prime_s11=NULL;
  newList(l2prime_s11);
  add(l1_s5, 1);
  add(l1_s5, 2);
  add(l1_s5, 3);
  add(l1_s5, 4);
  int  a_s13=0;
  copy(l1_s5, l2_s7, 1, a_s13);
  add(l1prime_s9, 1);
  add(l1prime_s9, 2);
  add(l1prime_s9, 3);
  add(l1prime_s9, 4);
  int  b_s15=0;
  copySKETCH(l1prime_s9, l2prime_s11, 1, b_s15);
  assert ((a_s13) == (b_s15));;
  int  a_s17=0;
  copy(l1_s5, l2_s7, 1, a_s17);
  int  b_s19=0;
  copySKETCH(l1prime_s9, l2prime_s11, 1, b_s19);
  assert ((a_s17) == (b_s19));;
  Node*  cur1=l1_s5->first;
  Node*  cur2=l1prime_s9->first;
  while ((cur1) != (NULL)) {
    assert ((cur1->val) == (cur2->val));;
    cur1 = cur1->next;
    cur2 = cur2->next;
  }
  assert ((cur2) == (NULL));;
  cur1 = l2_s7->first;
  cur2 = l2prime_s11->first;
  while ((cur1) != (NULL)) {
    assert ((cur1->val) == (cur2->val));;
    cur1 = cur1->next;
    cur2 = cur2->next;
  }
  assert ((cur2) == (NULL));;
}
void newList(List*& _out) {
  _out = NULL;
  _out = List::create(NULL, NULL);
  _out->first = NULL;
  _out->last = NULL;
  return;
}
void add(List* l, int newVal) {
  Node*  newN_s186=NULL;
  newNode(newN_s186);
  newN_s186->val = newVal;
  if ((l->first) == (NULL)) {
    l->first = newN_s186;
    l->last = newN_s186;
  } else {
    l->last->next = newN_s186;
    l->last = newN_s186;
  }
}
void copy(List* l1, List* l2, int idx, int& _out) {
  _out = 0;
  int  removedNode_s188=0;
  get(l1, idx, removedNode_s188);
  _out = removedNode_s188;
  add(l2, removedNode_s188);
  int  _out_s190=0;
  remove(l1, idx, _out_s190);
  return;
}
void copySKETCH(List* l1, List* l2, int z0, int& _out) {
  _out = 0;
  int  z3_s111=0;
  remove(l1, z0, z3_s111);
  add(l2, z3_s111);
  _out = z3_s111;
  return;
}
void newNode(Node*& _out) {
  _out = NULL;
  _out = Node::create(0, NULL);
  _out->next = NULL;
  return;
}
void get(List* l, int idx, int& _out) {
  _out = 0;
  Node*  cur=l->first;
  int  i=0;
  while ((cur) != (NULL)) {
    if ((i) == (idx)) {
      _out = cur->val;
      return;
    }
    cur = cur->next;
    i = i + 1;
  }
  _out = -100;
  return;
}
void remove(List* l, int idx, int& _out) {
  _out = 0;
  Node*  cur=l->first;
  Node*  toRemove=NULL;
  while ((cur) != (NULL)) {
    if ((0) == ((idx - 1))) {
      toRemove = cur->next;
      cur->next = cur->next->next;
      if ((l->last) == (toRemove)) {
        l->last = cur->next;
      }
      _out = toRemove->val;
      return;
    }
  }
  _out = -100;
  return;
}

}
