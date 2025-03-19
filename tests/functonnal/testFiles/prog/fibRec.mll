
int fib(int n) {
    si (n <= 1) {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}

int main() {
    int n = 10;
    int res = fib(n);
    retourne res;
}