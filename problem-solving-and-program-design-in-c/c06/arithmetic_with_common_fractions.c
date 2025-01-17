#include <stdio.h>
#include "fractionio.h"

int main(void) {
  int numerator, denominator;

  scan_fraction(&numerator, &denominator);
  
  printf("The number is %d / %d \n", numerator, denominator);

  return 0;
}
