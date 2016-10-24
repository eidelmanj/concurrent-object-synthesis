struct Node {
  int val;
  int key;
  Node next;
}


struct List {
  Node first;
  Node last;
}

  Node newNode() {
    Node newN = new Node();
    newN.next = null;
    return newN;
  }

List newList() {
  List newL = new List();
  newL.first = null;
  newL.last = null;
  return newL;
}




int push(List l, int key, int val) {
  Node cur = l.first;
  Node prev = l.first;

  if (cur == null) {
    Node newN = newNode();
    newN.key = key;
    newN.val = val;
    l.first = newN;
    return val;
  }
  
  
  while (cur != null && cur.key != key) {
    prev = cur;
    cur = cur.next;
  }

  if (cur != null) {
    cur.key = key;
    cur.val = val;
    return val;
  }

  Node newN = newNode();
  newN.key = key;
  newN.val = val;
  prev.next = newN;
  return val;
}


int get(List l, int key) {
  Node cur = l.first;

  while (cur != null && cur.key != key) {
    cur = cur.next;
  }

  if (cur == null) {
    return -100;
  }

  return cur.val;
  
}


int remove(List l, int key) {
  Node cur = l.first;

  if (cur == null) {
    return -100;
  }

  if (cur.key == key) {
     l.first = null;
     return cur.val;
  }

  Node prev = l.first;

  while (cur != null && cur.key != key) {
    prev = cur;
    cur = cur.next;
  }

  if (cur == null) {
    return -100;
  }

  int retVal = cur.val;
  prev.next = cur.next;

  return retVal;


  
}




  
/* int copy(List l1, List l2, int idx) { */
/*   int tmp; */
/*   tmp = get(l1, idx); */
/*   add(l2, tmp); */
/*   remove(l1, idx); */
  
/*   return tmp; */
/* } */




ORIGINAL

SKETCH




void checkList(List l1, List l2) {
  Node cur1 = l1.first;
  Node cur2 = l2.first;
  while (cur1!=null) {
    assert(cur1.key == cur2.key);
    assert(cur1.val == cur2.val);
    cur1 = cur1.next;
    cur2 = cur2.next;
  }
  assert(cur2 == null);
}



harness void blah() {
  List l1 = newList();
  List l2 = newList();

  List l1_2 = newList();
  List l2_2 = newList();


  int t1, t2;

  push(l1, 5, 6);
  push(l2, 6, 7);
  push(l2, 5, 2);
  push(l1, 2, 3);
  push(l2, 3, 4);
  push(l1, 4, 10);

  push(l1_2, 5, 6);
  push(l2_2, 6, 7);
  push(l2_2, 5, 2);
  push(l1_2, 2, 3);
  push(l2_2, 3, 4);

  push(l1_2, 4, 10);


  t1 = copy(l1, l2, 5);
  t2 = copySKETCH(l1_2, l2_2, 5);
  assert(t1 == t2);
  checkList(l1, l1_2);
  checkList(l2, l2_2);

  t1 = copy(l1, l2, 4);
  t2 = copySKETCH(l1_2, l2_2, 4);
  assert(t1 == t2);
  checkList(l1, l1_2);
  checkList(l2, l2_2);

  t1 = copy(l1, l2, 2);
  t2 = copySKETCH(l1_2, l2_2, 2);
  assert(t1 == t2);
  checkList(l1, l1_2);
  checkList(l2, l2_2);


  t1 = copy(l1, l2, 50);
  t2 = copySKETCH(l1_2, l2_2, 50);
  assert(t1 == t2);
  checkList(l1, l1_2);
  checkList(l2, l2_2);

  

  /*
  int t1 = copy(l1, 1);
  int t2 = copySKETCH(l2, 1);

  assert(t1 == t2);
  checkList(l1, l2);

  push(l1, 5, 6);
  push(l2, 5, 6);

  push(l1, 2, 3);
  push(l2, 2, 3);

  push(l1, 7, 10);
  push(l2, 7, 10);
  

  t1 = copy(l1, 1);
  t2 = copySKETCH(l2, 1);

  assert(t1 == t2);
  checkList(l1, l2);


  t1 = copy(l1, 5);
  t2 = copySKETCH(l2, 5);

  assert(t1 == t2);
  checkList(l1, l2);


  t1 = copy(l1, 2);
  t2 = copySKETCH(l2, 2);

  assert(t1 == t2);
  checkList(l1, l2); */




  


  

  /* List l1 = newList(); */
  /* List l2 = newList(); */

  /* List l1prime = newList(); */
  /* List l2prime = newList(); */

  /* add(l1, 1); */
  /* add(l1, 2); */
  /* add(l1, 3); */
  /* add(l1, 4); */

  /* int a = copy(l1, l2, 1); */


  /* add(l1prime, 1); */
  /* add(l1prime, 2); */
  /* add(l1prime, 3); */
  /* add(l1prime, 4); */

  /* int b = copySKETCH(l1prime, l2prime, 1); */
  /* assert(a == b); */

  /* a = copy(l1, l2, 1); */
  /* b = copySKETCH(l1prime, l2prime, 1); */

  /* assert(a == b); */

  /* Node cur1 = l1.first; */
  /* Node cur2 = l1prime.first; */
  /* while (cur1!=null) { */
  /*   assert(cur1.val == cur2.val); */
  /*   cur1 = cur1.next; */
  /*   cur2 = cur2.next; */
  /* } */
  /* assert(cur2 == null); */

  /* cur1 = l2.first; */
  /* cur2 = l2prime.first; */
  /* while (cur1!=null) { */
  /*   assert(cur1.val == cur2.val); */
  /*   cur1 = cur1.next; */
  /*   cur2 = cur2.next; */
  /* } */
  /* assert(cur2 == null); */
}  

  


  
