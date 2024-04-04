#include <stdio.h>

int
main(void)
{
  int i, j;
      printf("           I     J\n");
  for(i = 1; i < 4; ++i) {
      printf("Outer %6d \n", i);
    for(j = 0; j < i; ++j) {
      printf("   Inner    %6d \n", j);
    }
  }
  return 0;
}
