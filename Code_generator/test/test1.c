int a;
float b=3.5;
char c = 'x';
unsigned int d = 42;
const int e = 100;
struct Point {
    int x;
    int y;
};
int main() {
    float x = 10;
    int y = 20;
    printf("%f %d",x,y);
    x = x + y * 2;
    if (x > y) {
        x = x - 1;
    } 
    else if (x==y) {
        x=x+1;
    }
    return x;
}
