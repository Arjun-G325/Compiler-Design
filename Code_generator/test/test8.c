int factorial(int n) {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
}

int main() {
    int arr[] = {1, 2, 3, 4, 5};
    int result = factorial(arr[3]);
    return result;
}