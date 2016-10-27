[List l1, List l2][get, remove, push] [push ]
int copy(List l1, List l2,  int key) {

  int val;

  val = get(l1, key);

  if (val == -100) {
    putIfAbsent(l1, key, 6);
  }

  return val;

}
  
