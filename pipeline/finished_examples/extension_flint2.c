[List l1, List l2][get, remove, push, putIfAbsent] [push ]
int copy(List l1, List l2,  int key) {
  int val;

  int found;


  
  val = get(l1, key);

  if ( val != -100) {
    return 7;
  }

  else {
    int newV = 7;
    putIfAbsent(l1, key, newV);
    val = get(l1, key);
  }
  


  
  return val;
}
  
