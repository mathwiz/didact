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
    scan_fraction(&n1, &d1);
    op = get_operator();
    scan_fraction(&n2, &d2);

    switch (op) {
      case '+':
        add_fractions(n1, d1, n2, d2, &n_ans, &d_ans);
        break;
      case '-':
        add_fractions(n1, d1, -n2, d2, &n_ans, &d_ans);
        break;
      default:
        printf("Should not reach here. Received operator %c\n", op);
    }
    reduce_fraction(&n_ans, &d_ans);

    /* output result */
    printf("\n");
    print_fraction(n1, d1);
    printf(" %c ", op);
    print_fraction(n2, d2);
    printf(" = ");
    print_fraction(n_ans, d_ans);

    printf("\nDo another problem? (y/n)> ");
    scanf(" %c", &again);
  } while (again == 'y' || again == 'Y');

  return 0;
}
