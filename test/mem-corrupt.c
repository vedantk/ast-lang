#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
	/* Should flag an error. */
	char* buf = malloc(10);
	buf = realloc(buf, 20); /* No error checking. */

	/* No error here, move along. */
	char* buf2 = (char*) malloc(10);
	char* tmp = (char*) realloc(buf2, 40);
	if (!tmp) {
		free(buf2);
	}

	/* This could flag an error, but it's safe. */
	char* buf3 = malloc(10);
	char* tmp2 = buf3;
	buf3 = realloc(buf3, 80);
	if (!buf3) {
		free(tmp2);
	}

	/* Why would anyone do this? */
	char* buf4 = malloc(10);
	if (realloc(buf4, 160)) {
		free(buf4);
	}

	/* Stacks grow in different directions depending on your
	 * machine architecture. This isn't a reliable way to zero-out
	 * a set of variables! Next challenge?
	 */
	int a[2] = {1, 2}, b = 3;
	memset(a, 0, 12);
	printf("a1 = %d, a2 = %d, b = %d\n", a[0], a[1], b);

	int d[2] = {2, 3}, c = 1;
	memset(&c, 0, 12);
	printf("c = %d, d1 = %d, d2 = %d\n", c, d[0], d[1]);

	return 0;
}
