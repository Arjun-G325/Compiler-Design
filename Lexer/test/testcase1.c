int main() {
    int a;
    char b;
    float c;
    double d;
    void func();
    signed int x;
    unsigned int y;
    
    if (a == b) {
        return;
    } else if (x > y) {
        for (int i = 0; i < 10; ++i) {
            continue;
        }
    } else {
        while (x < y) {
            break;
        }
    }

    goto label;
    label: struct S { int m; };
    typedef int myint;
    static int s;
    const char* str;
    auto z = 42;
    default:
    switch (z) {
        case 1: break;
    }
}

