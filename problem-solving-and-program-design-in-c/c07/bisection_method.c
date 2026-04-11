#include <math.h>
#include <stdio.h>

#define TRUE 1
#define FALSE 0
#define PI 3.14159


double bisect(double xleft, double xright, double epsilon, double f(double arg), int *errp);
double g(double x);
double h(double x);


int
main(void) {

  double xleft, xright, epsilon, root;
  int error;

  printf("\nEnter interval endpoints> ");
  scanf("%lf%lf", &xleft, &xright);
  printf("\nEnter tolerance> ");
  scanf("lf", &epsilon);

  printf("\n\nFunction g");
  root = bisect(xleft, xright, epsilon, g, &error);
  if (!error) printf("\n g(%.7f) = %e]n", root, g(root));


  printf("%s\n", "sin");
  printf("%s\n", "cos");
  printf("%s\n", "tan");

  return 0;
}
