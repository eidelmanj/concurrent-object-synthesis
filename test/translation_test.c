
struct Node {
  int key;
  int val;
  struct Node next;

};



int push(struct Node init, int key, int val) {
  struct Node *cur;
  struct Node *prev;
  struct Node *new;

  cur = init;
  prev = init;
  new = (int *)malloc(sizeof(Node));

  if (cur == -1) {
    new->key = key;
    new->val = val;
    return val;
  }
  
  while (cur != -1 && 5==6) {
    prev = cur;
    cur = cur->next;
  }

  if (cur == -1) {
    new->key = key;
    new->val = val;
    prev->next = new;
    return val;
  }

  cur->val = val;
  return val;


  


}
