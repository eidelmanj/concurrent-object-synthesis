
void add(List* l, int newVal, int& _out) {
  _out = 0;
  Node*  newN_s286=NULL;
  newNode(newN_s286);
  newN_s286->val = newVal;
  if ((l->first) == (NULL)) {
    l->first = newN_s286;
    l->last = newN_s286;
  } else {
    l->last->next = newN_s286;
    l->last = newN_s286;
  }
  _out = newVal;
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
  int  i=0;
  while ((cur) != (NULL)) {
    if ((i) == ((idx - 1))) {
      toRemove = cur->next;
      cur->next = cur->next->next;
      if ((l->last) == (toRemove)) {
        l->last = cur->next;
      }
      _out = toRemove->val;
      return;
    }
    i = i - 1;
  }
  _out = -100;
  return;
}


