int sum(int n) {
    int s = 0;
    for (int i=0;i<n;i++) {
        s=s+i;
    }
    return s;
}

int main() {
    int total = sum(20);
    while (total > 0) {
        total = total - 1;
    }
    do {
        total = total + 2;
    } while (total < 5);

    return total;
}