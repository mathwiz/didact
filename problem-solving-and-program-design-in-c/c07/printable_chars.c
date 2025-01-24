#include <stdio.h>

#define START ' '
#define END 'Z'

int
main(void) {
  int code;
  printf("Printables[");
  for (code = (int)START; code <= (int)END; code++) {
    printf("%c", (char) code);
  }
  printf("]\n");
  return 0;
}
