#include <stdio.h>
#include "fractionio.h"
#include "mathio.h"
#include "mathlib.h"

int main(void) {
  /* first number */
  int n1, d1;
  /* second number */
  int n2, d2;
  /* answer */
  int n_ans, d_ans;
  /* inputs */
  char op, again;

  /* while the user wants to continue, repeat */
  do {
    printf("\nDo another problem? (y/n)> ");
    scanf(" %c", &again);
  } while (again == 'y' || again == 'Y');

  return 0;
}
