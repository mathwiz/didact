#include <math.h>
#include <stdio.h>

#define PI 3.14159


void
evaluate(double f(double f_arg), double x1, double x2, double x3) {
  printf("f(%.5f) = %.5f\n", x1, f(x1));
  printf("f(%.5f) = %.5f\n", x2, f(x2));
  printf("f(%.5f) = %.5f\n", x3, f(x3));
  printf("\n");
}


int
main(void) {

  printf("%s\n", "sin");
  evaluate(sin, PI/6, PI/4, PI/3);
  printf("%s\n", "cos");
  evaluate(cos, PI/6, PI/4, PI/3); 
  printf("%s\n", "tan");
  evaluate(tan, PI/6, PI/4, PI/3);

  return 0;
}
