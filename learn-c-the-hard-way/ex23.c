#include <stdio.h>
#include <string.h>
#include "dbg.h"

#define SIZE 1000

int normal_copy(char *from, char *to, int count) {
  int i = 0;
  for (i = 0; i < count; i++) to[i] = from[i];
  return i;
}

int duffs_device(char *from, char *to, int count) {
  {
    int n = (count + 7) / 8;
    switch (count % 8) {
    case 0:
      do {
      debug("duff n: %3d, char: %c", n, to[n]);
        *to++ = *from++;
        case 7:
        *to++ = *from++;
        case 6:
        *to++ = *from++;
        case 5:
        *to++ = *from++;
        case 4:
        *to++ = *from++;
        case 3:
        *to++ = *from++;
        case 2:
        *to++ = *from++;
        case 1:
        *to++ = *from++;
      } while (--n > 0);
    }
  }

  return count;
}

int zeds_device(char *from, char *to, int count) {
  {
    int n = (count + 7) / 8;
    switch (count % 8) {
        case 0:
again:  *to++ = *from++;
      debug(" zed n: %3d: char: %c", n, to[n]);

        case 7:
        *to++ = *from++;
        case 6:
        *to++ = *from++;
        case 5:
        *to++ = *from++;
        case 4:
        *to++ = *from++;
        case 3:
        *to++ = *from++;
        case 2:
        *to++ = *from++;
        case 1:
        *to++ = *from++;
        if (--n > 0) goto again;
    }
  }

  return count;
}

int valid_copy(char *data, int count, char expects) {
  int i = 0;
  for (i = 0; i < count; i++) {
    if (data[i] != expects) {
      log_err("[pos in data %d] '%c' (from) != '%c' (expected)", i, data[i], expects);
      return 0;
    }
  }
  return 1;
}

int main(int argc, char *argv[]) {
  char from[SIZE] = { 'a' };
  char to[SIZE] = { 'c' };
  int rc = 0;

  // set up arrays
  memset(from, 'x', SIZE);
  memset(to, 'y', SIZE);
  check(valid_copy(to, SIZE, 'y'), "Not initialized correctly.");

  // normal copy
  rc = normal_copy(from, to, SIZE);
  check(rc == SIZE, "Normal copy failed: %d", rc);
  check(valid_copy(to, SIZE, 'x'), "Normal copy failed.");

  // reset
  memset(to, 'y', SIZE);

  // duff
  printf("to before Duff\n");
  printf("%s\n", to);
  rc = duffs_device(from, to, SIZE);
  check(rc == SIZE, "Duff's copy failed: %d", rc);
  check(valid_copy(to, SIZE, 'x'), "Duff's copy failed.");
  printf("to after Duff\n");
  printf("%s\n", to);

  // reset
  memset(to, 'y', SIZE);

  // zed
  printf("to before Zed\n");
  printf("%s\n", to);
  rc = zeds_device(from, to, SIZE);
  check(rc == SIZE, "Zed's copy failed: %d", rc);
  check(valid_copy(to, SIZE, 'x'), "Zed's copy failed.");
  printf("to after Zed\n");
  printf("%s\n", to);

  // reset
  memset(to, 'y', SIZE);


  return 0;
error:
  return 1;
}
