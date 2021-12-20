
int f();

int ssa2() {
	int y, z;
	y =f();
	if (y < 0) {
		z = y + 1;
	} else {
		z = y + 2;
	}
	return z;
}
