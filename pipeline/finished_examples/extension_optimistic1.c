[List l1, List l2][get, remove, push] [push ]
int copy(List l1, List l2,  int key) {
  int val1;
  int val2;
  val1 = get(l1, key);



  if (val1 != -100) {
    val2 = push(l2, key, val1);
    
    return val2;

  }

  return -100;
}
  
