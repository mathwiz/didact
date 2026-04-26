#include <stdio.h>
#include <ctype.h>
#include "dbg.h"

#define SUCCESS_CODE 0
#define ERROR_CODE 1

int print_a_message(const char *msg) {
  printf("A STRING: %s\n", msg);
  return SUCCESS_CODE;
}

int uppercase(const char *msg) {
  int i = 0;
  // BUG: \0 termination problems
  for (i = 0; msg[i] != '\0'; i++)
    printf("%c", toupper(msg[i]));

  printf("\n");
  return SUCCESS_CODE;
}

int lowercase(const char *msg) {
  int i = 0;
  // BUG: \0 termination problems
  for (i = 0; msg[i] != '\0'; i++)
    printf("%c", tolower(msg[i]));

  printf("\n");
  return SUCCESS_CODE;
}

int fail_on_purpose(const char *msg) {
  return ERROR_CODE;
}
