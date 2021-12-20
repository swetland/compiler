
struct s {
	int a;
	int b;
	int c;
};

void func(struct s* x, int y) {
	x->a = y;
	x->b = y * 2;
	x->c = y - 45;
}
