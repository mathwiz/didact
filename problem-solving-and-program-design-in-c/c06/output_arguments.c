#include <stdio.h>
#include <math.h>

void separate_number(double num, char *signp, int *wholep, double *fracp);

int main(void) {
  double value; 
  char sign;
  int whole_part;
  double frac_part;

  printf("Enter a value to analyze> ");
  scanf("%lf", &value);

  separate_number(value, &sign, &whole_part, &frac_part);

  printf("Parts of %.4f\n sign: %c\n", value, sign);
  printf(" whole part: %d\n", whole_part);
  printf(" fractional part: %.4f\n", frac_part);

  return 0;
}


void separate_number(double num, char *signp, int *wholep, double *fracp) {
  double magnitude;
  if (num < 0.0) *signp = '-';
  else if (num > 0.0) *signp = '+';
  else *signp = ' ';

  magnitude = fabs(num);
  *wholep = floor(magnitude);
  *fracp = magnitude - *wholep;
}
