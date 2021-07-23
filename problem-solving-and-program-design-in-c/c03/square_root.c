/*
 * Performs 3 square root computations
 */

#include <math.h>
#include <stdio.h>

int
main(void)
{
  double first, second;

  printf("Enter the first number> ");
  scanf("%lf", &first);
  printf("The square root of the first number is %.4f.\n", sqrt(first));

  printf("Enter the second number> ");
  scanf("%lf", &second);
  printf("The square root of the second number is %.4f.\n", sqrt(second));

  printf("The square root of the sum of the numbers is %.4f.\n", sqrt(first + second));

  return 0;
}
