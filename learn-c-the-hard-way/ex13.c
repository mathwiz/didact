#include <stdio.h>

int main(int argc, char *argv[])
{
  int i = 0;
  int j = 0;

  printf("Array of char arrays\n");
  for (i = 0; i < argc; i++) {
    printf("arg %d: %s\n", i, argv[i]);
  }

  printf("Array of char arrays defined from strings\n");
  int n_states = 4;
  char *states[] = {
    "California", "Texas", "Florida", "New York",
  };
  for (i = 0; i < n_states; i++) {
    printf("State %d: ", i);
    for (j = 0; states[i][j] != '\0'; j++)
      printf("%c ", states[i][j]);
    printf("\n");
  }

  return 0;
}
