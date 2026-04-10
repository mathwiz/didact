#include <stdio.h>
#include <ctype.h>

//forward declarations

int can_print(char c);
void print_letters(char c[]);

void print_arguments(int argc, char *argv[]) {
  int i = 0;
  if (argc == 1) return;
  for (i = 1; i < argc; i++)
    print_letters(argv[i]);
}

void print_letters(char string[]) {
  int i = 0;
  for (i = 0; string[i] != '\0'; i++) {
    char c = string[i];
    if (can_print(c))
      printf("'%c' = %d ", c, c);
  }
  printf("\n");
}

int can_print(char c) {
  return isalpha(c) || isnumber(c) || isblank(c);
}

int main(int argc, char *argv[])
{
  print_arguments(argc, argv);
  return 0;
}
