[List l1, List l2][get, remove, push, putIfAbsent, compute] [push ]
int copy(List l1, List l2,  int key) {
  int val1;
  int val2;
  val1 = get(l1, key);



  if (val1 == -100) {

    val2 = compute(val1);
    putIfAbsent(l1, key, val2);
    
    return val2;

  }

  return -100;
}
  
