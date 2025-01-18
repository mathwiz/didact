#include <stdio.h>

char get_operator(void);

/* ----------------- */

char 
get_operator() {
  char op;
  printf("Enter an arithmatic operator (+, -, *, /)\n> ");
  for (scanf("%c", &op); 
       op != '+' && op != '-' && op != '*' && op != '/';
       scanf("%c", &op))
    {
      if (op != '\n')
        printf("%c invalid. Reenter operator (+, -, *, /)\n> ", op);
    }
  return op;
}

