#include <stdio.h>

int main(int argc, const char * argv[])
{
  FILE *inp;
  int sum = 0, score, input_status;
  
  inp = fopen(argv[1], "r");
  input_status = fscanf(inp, "%d", &score);
  printf("Scores\n");

  while (input_status != EOF) {
    printf("%4d\n", score);
    sum += score;
    input_status = fscanf(inp, "%d", &score);
  }

  printf("\nSum of exam scores is %d\n", sum);
  fclose(inp);

  return 0;
}
