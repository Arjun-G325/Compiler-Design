int square(int n) {
    return n * n;
}

int add(int a, int b, int c) {
    return a + b + c;
}

int main() {
    int result = square(5);
    int total = add(1, 2, 3);

    auto lam = [](int x) {
        return x + 10;
    };

    int v = lam(result, 100);    
    return total + v;
}