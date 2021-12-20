int fib(int n) {
    int a = 0;
    int b = 1;
    int z = 0;
    while (n != z) {
        int t = a + b;
        a = b;
        b = t;
        n = n - 1;
        z = 0;
    }
    return a;
}

