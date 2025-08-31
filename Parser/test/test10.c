typedef union Data {
    int i;
    float f;
    char str[20];
} Data;

enum Colors { RED, GREEN, BLUE };

int main() {
    Data d;
    d.f = 3.14;
    enum Colors c = BLUE;
    return c;
}
