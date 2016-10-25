[List l1][get, remove, push] [push ]
int copy(List l1, int key) {
  int found;
  int val;
  found = get(l1, key);
  val = -100;
  if (found != -100) {
    val = get(l1, key);
    remove(l1, key);
  }
  return val;
}
  
