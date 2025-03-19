int fibIter(int n) {
    if (n <= 1) {
        return n;
    }
    int a = 0;
    int b = 1;
    int c = 0;
    pour (int i = 2; i <= n; i++) {
        c = a + b;
        a = b;
        b = c;
    }
    return b;
}

int main() {
    int n = 10;
    int res = fibIter(n);
    return res;
}