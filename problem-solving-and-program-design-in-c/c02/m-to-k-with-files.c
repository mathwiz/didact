/* Converts distances from miles to kilometers */

#include <stdio.h>

#define KMS_PER_MILE 1.609      /* conversion constant */

int main(void) {
  double miles, kms;
  FILE *inp, *outp;             /* pointers to files */

  inp = fopen("distance.dat", "r");
  outp = fopen("distance.out", "w");

  fscanf(inp, "%lf", &miles);
  fprintf(outp, "The distance in miles is %.2f.\n", miles);
  
  kms = KMS_PER_MILE * miles;

  fprintf(outp, "That equals %.2f kilometers.\n", kms);

  /* close files */
  fclose(inp);
  fclose(outp);

  return 0;
}
