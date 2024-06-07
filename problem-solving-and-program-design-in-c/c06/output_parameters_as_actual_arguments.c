#include <stdio.h>

void scan_fraction(int *nump, int *denomp);

int main(void) {
  int numerator, denominator;

  scan_fraction(&numerator, &denominator);
  
  printf("The number is %d / %d \n", numerator, denominator);

  return 0;
}

void scan_fraction(int *nump, int *denomp) {
  char slash;
  int status;
  int error;
  char discard;

  do {
    error = 0; /* no error yet */
    printf("Enter a common fraction as two integers separated ");
    printf("by a slash> ");
    status = scanf("%d %c%d", nump, &slash, denomp);

    if (status < 3) {
      error = 1;
      printf("Invalid: Please read directions.\n");
    } else if (slash != '/') {
      error = 1;
      printf("Invalid: Please use '/' as the separator.\n");
    } else if (*denomp <= 0) {
      error = 1;
      printf("Invalid: Denominator must be positive.\n");
    }

    do {
      scanf("%c", &discard);
    } while (discard != '\n');

  } while (error);

}
