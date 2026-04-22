#undef NDEBUG
#include <stdio.h>
#include <assert.h>
#include "dbg.h"

#define SUCCESS_CODE 0
#define ERROR_CODE -1

/*
 * Naive copy that assumes all inputs are valid.
 */
void copy(char to[], char from[]) {
  int i = 0;
  //loop will not terminate if from is not '\0' terminated
  while ((to[i] = from[i]) != '\0') i++;
}

/*
 * Safer version that checks the lengths of each string
 */
int safercopy(int from_len, char *from, int to_len, char *to) {
  assert(from != NULL && to != NULL && "from and to cannot be NULL");
  int i = 0;
  int max = from_len > to_len - 1 ? to_len - 1 : from_len;

  // to_len must be at least 1 byte
  if (from_len < 0 || to_len <= 0) return ERROR_CODE;

  for (i = 0; i < max; i++)
    to[i] = from[i];

  to[to_len - 1]= '\0';

  return i;
}

int main(int argc, char *argv[])
{
  // understand why we can get these sizes
  char from[] = "abcdefghij";
  int from_len = sizeof(from);

  // notice it is 7 chars + '\0'
  char to[] = "0123456";
  int to_len = sizeof(to);

  debug("Copying '%s':%d to '%s':%d", from, from_len, to, to_len);

  int rc = safercopy(from_len, from, to_len, to);
  check(rc > 0, "safercopy failed");
  check(to[to_len - 1] == '\0', "String not terminated");

  debug("Result is: '%s':%d", to, to_len);

  // now try to break it
  rc = safercopy(from_len * -1, from, to_len, to);
  check(rc = ERROR_CODE, "safercopy should fail #1");
  check(to[to_len - 1] == '\0', "String not terminated");

  rc = safercopy(from_len, from, 0, to);
  check(rc = ERROR_CODE, "safercopy should fail #2");
  check(to[to_len - 1] == '\0', "String not terminated");


  return SUCCESS_CODE;
error:
  return ERROR_CODE;
}
