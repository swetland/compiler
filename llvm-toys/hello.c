int func(int x, int y) {
	while (x > 0) {
		y = y + x;
		if (x == 17) {
			y *= 2;
		} else {
			y -= 1;
		}
		x = x - 1;
	}
	return y;
}

