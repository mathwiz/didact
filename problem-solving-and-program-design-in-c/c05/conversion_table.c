#include <stdio.h>

#define CBEGIN 40
#define CLIMIT -25
#define CSTEP 5

int main(int argc, const char * argv[])
{
  int celsius;
  double fahrenheit;

  printf("      Celsius    Fahrenheit\n");

  for (celsius = CBEGIN;
       celsius >= CLIMIT;
       celsius -= CSTEP) {
    fahrenheit = 1.8 * celsius + 32.0;
    printf("%6c%3d%8c%7.2f\n", ' ', celsius, ' ', fahrenheit);
  }
  
  return 0;
}
