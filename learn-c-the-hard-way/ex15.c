#include <stdio.h>

int main(int argc, char *argv[])
{
  int ages[] = { 10, 12, 13, 14, 20 };
  char *names[] = {
    "Ali", "Bashir", "Crystal", "Dina", "Jasmine"
  };
    
  //safely obtain the size of an array
  int count = sizeof(ages) / sizeof(int);
  int i = 0;

  for (i = 0; i < count; i++)
    printf("%s has %d years alive.\n", names[i], ages[i]);

  printf("----\n");

  // pointers to start of arrays
  int *cur_age = ages;
  char **cur_name = names;

  for (i = 0; i < count; i++)
    printf("%s has %d years alive if you believe pointers.\n", 
           *(cur_name + i), *(cur_age + i));

  printf("----\n");

  for (i = 0; i < count; i++)
    printf("%s has %d years alive if you like array access of pointers.\n", 
           cur_name[i], cur_age[i]);

  printf("----\n");

  for (cur_name = names, cur_age = ages;
       (cur_age - ages) < count; 
       cur_name++, cur_age++) {
    printf("(cur_age - ages) = (%p - %p) = %d\n", cur_age, ages, (cur_age - ages));
    printf("%s has %d years alive if you like complex array access.\n", *cur_name, *cur_age);
  }

  return 0;
}
