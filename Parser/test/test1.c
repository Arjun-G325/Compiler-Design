int a;
float b = 3.5;
char c = 'x';
unsigned int d = 42;
const int e = 100;

int main() {
    int x = 10, y = 20;
    x = x + y * 2;
    if (x > y) {
        x = x - 1;
    } else {
        x = x + 1;
    }
    return x;
}

