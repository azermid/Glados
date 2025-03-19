float cos(float x) {
    float res = 1 - x*x/2 + x*x*x*x/24 - x*x*x*x*x*x/720 + x*x*x*x*x*x*x*x/40320 - x*x*x*x*x*x*x*x*x*x/3628800 + x*x*x*x*x*x*x*x*x*x*x*x/479001600;
    return res;
}



int main() {
    for (int i = 0; i < 10; i = i + 1) {
        int x = 0;
    }
    return 0;
}
