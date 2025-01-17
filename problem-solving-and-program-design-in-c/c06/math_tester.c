#include <stdio.h>
#include "mathio.h"
#include "mathlib.h"

int main(void) {
  int status;
  int numerator, denominator;
  int gcd;

  printf("Enter a common fraction as two integers separated by a space> ");
  status = scanf("%d %d", &numerator, &denominator);

  gcd = find_gcd(numerator, denominator);

  printf("The answer is %d / %d / %d \n", numerator, denominator, gcd);

  return 0;
}
