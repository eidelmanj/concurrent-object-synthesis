[List l1, List l2][get, remove, push] [push ]
int copy(List l1, List l2,  int key) {
  int val;

  boolean found;

  val = -100;
  
  found = contains(l1, key);
  if (found) {
    val = get(l1, key);
    remove(l1, key);
  }


  
  return val;
}
  
