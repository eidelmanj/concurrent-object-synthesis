#include <cstdio>
#include <assert.h>
#include <iostream>
using namespace std;
#include "vops.h"
#include "outComplete2.h"
namespace ANONYMOUS{

Node* Node::create(int  val_, int  key_, Node*  next_){
  void* temp= malloc( sizeof(Node)  ); 
  Node* rv = new (temp)Node();
  rv->val =  val_;
  rv->key =  key_;
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
  List*  l1_s19=NULL;
  newList(l1_s19);
  List*  l2_s21=NULL;
  newList(l2_s21);
  List*  l1_2_s23=NULL;
  newList(l1_2_s23);
  List*  l2_2_s25=NULL;
  newList(l2_2_s25);
  int  _out_s27=0;
  push(l1_s19, 5, 6, _out_s27);
  int  _out_s29=0;
  push(l2_s21, 6, 7, _out_s29);
  int  _out_s31=0;
  push(l2_s21, 5, 2, _out_s31);
  int  _out_s33=0;
  push(l1_s19, 2, 3, _out_s33);
  int  _out_s35=0;
  push(l2_s21, 3, 4, _out_s35);
  int  _out_s37=0;
  push(l1_s19, 4, 10, _out_s37);
  int  _out_s39=0;
  push(l1_2_s23, 5, 6, _out_s39);
  int  _out_s41=0;
  push(l2_2_s25, 6, 7, _out_s41);
  int  _out_s43=0;
  push(l2_2_s25, 5, 2, _out_s43);
  int  _out_s45=0;
  push(l1_2_s23, 2, 3, _out_s45);
  int  _out_s47=0;
  push(l2_2_s25, 3, 4, _out_s47);
  int  _out_s49=0;
  push(l1_2_s23, 4, 10, _out_s49);
  int  t1_s51=0;
  copy(l1_s19, l2_s21, 5, t1_s51);
  int  t2_s53=0;
  copySKETCH(l1_2_s23, l2_2_s25, 5, t2_s53);
  assert ((t1_s51) == (t2_s53));;
  checkList(l1_s19, l1_2_s23);
  checkList(l2_s21, l2_2_s25);
  int  t1_s55=0;
  copy(l1_s19, l2_s21, 4, t1_s55);
  int  t2_s57=0;
  copySKETCH(l1_2_s23, l2_2_s25, 4, t2_s57);
  assert ((t1_s55) == (t2_s57));;
  checkList(l1_s19, l1_2_s23);
  checkList(l2_s21, l2_2_s25);
  int  t1_s59=0;
  copy(l1_s19, l2_s21, 2, t1_s59);
  int  t2_s61=0;
  copySKETCH(l1_2_s23, l2_2_s25, 2, t2_s61);
  assert ((t1_s59) == (t2_s61));;
  checkList(l1_s19, l1_2_s23);
  checkList(l2_s21, l2_2_s25);
  int  t1_s63=0;
  copy(l1_s19, l2_s21, 50, t1_s63);
  int  t2_s65=0;
  copySKETCH(l1_2_s23, l2_2_s25, 50, t2_s65);
  assert ((t1_s63) == (t2_s65));;
  checkList(l1_s19, l1_2_s23);
  checkList(l2_s21, l2_2_s25);
}
void newList(List*& _out) {
  _out = NULL;
  _out = List::create(NULL, NULL);
  _out->first = NULL;
  _out->last = NULL;
  return;
}
void push(List* l, int key, int val, int& _out) {
  _out = 0;
  Node*  cur=l->first;
  Node*  prev=l->first;
  if ((cur) == (NULL)) {
    Node*  newN_s370=NULL;
    newNode(newN_s370);
    newN_s370->key = key;
    newN_s370->val = val;
    l->first = newN_s370;
    _out = val;
    return;
  }
  bool  __sa0=((cur) != (NULL)) && ((cur->key) != (key));
  while (__sa0) {
    prev = cur;
    cur = cur->next;
    __sa0 = ((cur) != (NULL)) && ((cur->key) != (key));
  }
  if ((cur) != (NULL)) {
    cur->key = key;
    cur->val = val;
    _out = val;
    return;
  }
  Node*  newN_s372=NULL;
  newNode(newN_s372);
  newN_s372->key = key;
  newN_s372->val = val;
  prev->next = newN_s372;
  _out = val;
  return;
}
void copy(List* l1, List* l2, int key, int& _out) {
  _out = 0;
  int  val1_s374=0;
  get(l1, key, val1_s374);
  if ((val1_s374) != (-100)) {
    int  _out_s376=0;
    remove(l2, key, _out_s376);
    int  val2_s378=0;
    push(l2, key, val1_s374, val2_s378);
    _out = val2_s378;
    return;
  }
  _out = -100;
  return;
}
void copySKETCH(List* l1, List* l2, int z0_0, int& _out) {
  _out = 0;
  int  z3_s77=0;
  get(l1, z0_0, z3_s77);
  if ((z3_s77) != (-(100))) {
    int  z4_s207=0;
    remove(l2, z0_0, z4_s207);
  }
  if ((z3_s77) != (-(100))) {
    int  z5_s327=0;
    push(l2, z0_0, z3_s77, z5_s327);
  }
  _out = z3_s77;
  return;
}
void checkList(List* l1, List* l2) {
  Node*  cur1=l1->first;
  Node*  cur2=l2->first;
  while ((cur1) != (NULL)) {
    assert ((cur1->key) == (cur2->key));;
    assert ((cur1->val) == (cur2->val));;
    cur1 = cur1->next;
    cur2 = cur2->next;
  }
  assert ((cur2) == (NULL));;
}
void newNode(Node*& _out) {
  _out = NULL;
  _out = Node::create(0, 0, NULL);
  _out->next = NULL;
  return;
}
void get(List* l, int key, int& _out) {
  _out = 0;
  Node*  cur=l->first;
  bool  __sa1=((cur) != (NULL)) && ((cur->key) != (key));
  while (__sa1) {
    cur = cur->next;
    __sa1 = ((cur) != (NULL)) && ((cur->key) != (key));
  }
  if ((cur) == (NULL)) {
    _out = -100;
    return;
  }
  _out = cur->val;
  return;
}
void remove(List* l, int key, int& _out) {
  _out = 0;
  Node*  cur=l->first;
  if ((cur) == (NULL)) {
    _out = -100;
    return;
  }
  if ((cur->key) == (key)) {
    l->first = NULL;
    _out = cur->val;
    return;
  }
  Node*  prev=l->first;
  bool  __sa2=((cur) != (NULL)) && ((cur->key) != (key));
  while (__sa2) {
    prev = cur;
    cur = cur->next;
    __sa2 = ((cur) != (NULL)) && ((cur->key) != (key));
  }
  if ((cur) == (NULL)) {
    _out = -100;
    return;
  }
  int  retVal=cur->val;
  prev->next = cur->next;
  _out = retVal;
  return;
}

}
