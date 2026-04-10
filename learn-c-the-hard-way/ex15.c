#include <stdio.h>

int main(int argc, char *argv[])
{
  int ages[] = { 10, 12, 13, 14, 20 };
  char *names[] = {
    "Ali", "Baboucar", "Crystal", "Diana"
  };
    
  //safely obtain the size of an array
  int count = sizeof(ages) / sizeof(int);
  int i = 0;

  return 0;
}
