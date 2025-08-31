int main() {
    int a = 5, b = 10, c = 0;
    c = (a & b) | (a ^ b);
    
    while (c < 50) c += 5;

    do {
        c--;
    } while (c > 20);

    for (int i = 0; i < 5; i++) {
        c += i;
    }

    return c;
}
