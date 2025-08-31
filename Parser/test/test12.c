int add(int a, int b) { return a + b; }
int mul(int a, int b) { return a * b; }

int main() {
    int (*fptr)(int, int);
    int x = 10, y = 20;
    fptr = (x > y) ? add : mul;
    return fptr(x, y);
}
