#include <stdio.h>

int main(int argc, char *argv[])
{
  int nums[4] = { 0 }; // single initializer fills rest with 0
  char name[4] = { 0 }; // single initializer fills rest with 0

  printf("after initialization\n");
  printf("numbers: %d %d %d %d\n", nums[0], nums[1], nums[2], nums[3]);
  printf("name: %c %c %c %c\n", name[0], name[1], name[2], name[3]);
  printf("name as string: %s\n", name);

  nums[0] = 1;
  nums[1] = 2;
  nums[2] = 3;
  nums[3] = 4;

  name[0] = 'Z';
  name[1] = 'e';
  name[2] = 'd';
  name[3] = '\0';

  printf("after assigning\n");
  printf("numbers: %d %d %d %d\n", nums[0], nums[1], nums[2], nums[3]);
  printf("name: %c %c %c %c\n", name[0], name[1], name[2], name[3]);
  printf("name as string: %s\n", name);

  char *another = "Sha";
  printf("another as string: %s\n", another);
  printf("another as array: %c %c %c %c\n", 
         another[0], another[1], another[2], another[3]);

  return 0;
}
