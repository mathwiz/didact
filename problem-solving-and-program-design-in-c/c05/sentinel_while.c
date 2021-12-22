#include <stdio.h>

#define SENTINEL -99

int main(int argc, const char * argv[])
{
  int sum = 0, score;

  printf("Enter first score (or %d to quit)> ", SENTINEL);
  scanf("%d", &score);
  
  while (score != SENTINEL) {
    sum += score;
    printf("Enter next score (or %d to quit)> ", SENTINEL);
    scanf("%d", &score);
  }

  printf("\nSume of exam scores is %d\n", sum);
  
  return 0;
}
