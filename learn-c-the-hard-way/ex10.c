#include <stdio.h>

int main(int argc, char *argv[])
{
  if (argc != 2) {
    printf("ERROR: Only one argument needed.\n");
    return 1;
  }

  int i = 0;
  for (i = 0; argv[1][i] != '\0'; i++) {
    char letter = argv[1][i];
    
    switch (letter) {
      case 'a':
      case 'A':
        printf("%d: 'a'\n", i);
        break;
      case 'e':
      case 'E':
        printf("%d: 'e'\n", i);
        break;
      case 'i':
      case 'I':
        printf("%d: 'i'\n", i);
        break;
      case 'o':
      case 'O':
        printf("%d: 'o'\n", i);
        break;
      case 'u':
      case 'U':
        printf("%d: 'y'\n", i);
        break;
      case 'y':
      case 'Y':
        if (i > 0) {
          printf("%d: 'y'\n", i);
        } else {
          printf("%d: 'y' is not a vowel\n", i);
        }
        break;
      default:
        printf("%d: '%c' is not a vowel\n", i, letter);
    }
  } 

  return 0;
}
