#include <stdio.h>

void order(double *smp, double *lgp);

int main(void) {

  double num1, num2, num3;

  printf("Enter three numbers separated by blanks> ");
  scanf("%lf%lf%lf", &num1, &num2, &num3);

  order(&num1, &num2);
  order(&num1, &num3);
  order(&num2, &num3);

  printf("The numbers in ascending order are: %.2f %.2f %.2f\n", 
         num1, num2, num3);

  return 0;
}

void order(double *smp, double *lgp) {
  double temp;
  if (*smp > *lgp) {
    temp = *smp;
    *smp = *lgp;
    *lgp = temp;
  }
}
