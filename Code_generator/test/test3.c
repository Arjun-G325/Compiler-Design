
struct Point {
    int x;
    int y;
};

union Data {
    int i;
    float f;
};

enum Color {
    RED,
    GREEN,
    BLUE
};

int main() {
    struct Point p;
    p.x = 10;
    p.y = 20;
    union Data d;
    d.f = 3.14;
    return p.x;
}