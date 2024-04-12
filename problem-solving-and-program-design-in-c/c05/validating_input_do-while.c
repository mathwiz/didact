#include <stdio.h>

int get_int(int n_min, int n_max) {
  int in_val, status;
  char skip_ch;
  int error;

  do {
    error = 0; /* no error yet */
    
    /* get number from user */
    printf("Enter an integer in the range from %d ", n_min);
    printf("to %d inclusive> ", n_max);
    status = scanf("%d", &in_val);

    /* validate */
    if (status != 1) { /* input was not a number */
      error = 1;
      scanf("%c", &skip_ch);
      printf("Invalid character >>%c>>. ", skip_ch);
      printf("Skipping rest of line.\n");
    } else if (in_val < n_min || in_val > n_max) {
      error = 1;
      printf("Number %d is not in range.\n", in_val);
    }

    /* skip the rest of data line */
    do {
      scanf("%c", &skip_ch);
      if (skip_ch != '\n') printf("Skipping >>%c>>\n", skip_ch);
    }
    while (skip_ch != '\n');

  } while (error);

  return (in_val);
}

int main(int argc, const char * argv[])
{
  int min = 10;
  int max = 99;

  printf("Validating input is between %d and %d\n", min, max);

  int x = get_int(min, max);
  printf("Valid input is %d\n", x);

  return 0;
}
