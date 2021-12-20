
int func(int a, int b);

int caller(int n, int a, int b) {
	int r;
	while (n > 0) {
		int x = func(a * n, b + n);
		r = r + x;
		n--;
	}
	return r;
}
