/*
 * Test a function taking multiple arguments.
 */

#include <stdio.h>
#include <math.h>


double scale(double num_to_scale, int scale_factor);


int
main(void)
{
  double num;
  int factor;

  printf("Enter a real number to scale> ");
  scanf("%lf", &num);

  printf("Enter an integer scale factor> ");
  scanf("%d", &factor);
  
  printf("Result of call to scale(%f, %d) is %f\n", num, factor, scale(num, factor));

  return 0;
}


double
scale(double x, int n)
{
  return x * pow(10, n);
}
