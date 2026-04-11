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
  scanf("%lf", &epsilon);

  printf("\nFunction g");
  root = bisect(xleft, xright, epsilon, g, &error);
  if (!error) printf("\n g(%.7f) = %e\n", root, g(root));

  printf("\nFunction h");
  root = bisect(xleft, xright, epsilon, h, &error);
  if (!error) printf("\n h(%.7f) = %e\n", root, h(root));

  printf("done.\n");

  return 0;
}


/*
* Finds a root of function f. 
* If signs of f(xleft) and f(xright) are opposite, set errp to FALSE.
* Otherwise error is TRUE.
*/
double bisect(double xleft, double xright, double epsilon, double f(double arg), int *errp) {
  double 
    xmid,
    fleft,  // f(xleft)
    fright, // f(xright)
    fmid;   // f(xmid)
  int root_found = FALSE;
  
  //initial endpoints
  fleft = f(xleft);
  fright = f(xright);

  if (fleft * fright > 0) {  //same sign
    *errp = TRUE;
    printf("\nMay be no root in [%.7f, %.7f]\n", xleft, xright);
  } else {
    //search as long as interval is large enough and no root is found
    while (fabs(xright - xleft) > epsilon && !root_found) {
      xmid = (xleft + xright) / 2.0;
      fmid = f(xmid);
      if (fmid == 0.0)
        root_found = TRUE;
      else if (fleft * fmid < 0.0)  // root in [xleft, xmid]
        xright = xmid;
      else {
        xleft = xmid;
        fleft = fmid;
      }

      if (root_found) {
        printf("\nRoot found at x = %.7f, midpoint of [%.7f, %.7f]",
               xmid, xleft, xright);
      } else {
        printf("\nNew interval is [%.7f, %.7f]", xleft, xright);
      } 
    }
    printf("\n");
  }

  //if there is a root, it is midpoint of [xleft, xright]
  return (xleft + xright) / 2.0;
}


/*  3     2
* 5x  - 2x  + 3
*/
double g(double x) {
  return (5 * pow(x, 3.0) - 2 * pow(x, 2.0) + 3);
} 


/*  4     2
*  x  - 3x  - 8
*/
double h(double x) {
  return (1 * pow(x, 4.0) - 3 * pow(x, 2.0) - 8);
} 
