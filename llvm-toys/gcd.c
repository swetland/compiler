int gcd(int x1, int x2) {
    while (x2 != 0) {
        int q = x1 / x2;
        int t = q * x2;
        int r = x1 - t;
        x1 = x2;
        x2 = r;
    }
    return x1;
}

