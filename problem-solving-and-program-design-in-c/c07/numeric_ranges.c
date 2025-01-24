#include <stdio.h>
#include <limits.h>
#include <float.h>

int
main(void) {
  printf("Range of values of type int: %d . . %d\n", INT_MIN, INT_MAX);
  printf("Range of positive values of type double: %e . . %e\n", DBL_MIN, DBL_MAX);

  return 0;
}
