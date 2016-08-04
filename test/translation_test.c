struct bob {
    int x;
    char y;

}; 
int compare(int x, char y, int z) {
    if(x > z) {
        char y = 1;
    } else {
        char y = 0; 
    }
}
void pointless_loop() {
    int x = 0; 
    int lock; 
    pthhread_mutex_lock(&lock);
    while(x <= 10) {
        x = x + 1;
    }
}
    
void pointless_bool() {
    int x = 0;
    int y = 0;
    if(x && y) {  
    }
}
