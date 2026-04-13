#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

void die(const char *message) {
  if (errno) {
    perror(message);
  } else {
    printf("ERROR: %s\n", message);
  }
  exit(1);
}

typedef int (*compare_delagate) (int a, int b);

int *bubble_sort(int *nums, int n, compare_delagate comparator) {
  int temp = 0;
  int i = 0;
  int j = 0;
  int *target = malloc(n * sizeof(int));

  if (!target) die("Memory error in bubble_sort");

  memcpy(target, nums, n * sizeof(int));

  for (i = 0; i < n; i++) {
    for (j = 0; j < n - 1; j++) {
      if (comparator(target[j], target[j + 1]) > 0) {
          temp = target[j + 1];
          target[j + 1] = target[j];
          target[j] = temp;
      }
    }
  }

  return target;
}

void clean_up(int *elems) {
  free(elems);
}

int ascending(int a, int b) {
  return a - b;
}

int descending(int a, int b) {
  return b - a;
}

int strange_order(int a, int b) {
  if (a == b || b == 0) return 0;
  return a % b;
}

void test_sorting(int *nums, int n, compare_delagate comparator) {
  int i = 0;
  int *sorted = bubble_sort(nums, n, comparator);

  if (!sorted) die("Sort failed!");

  for (i = 0; i < n; i++) {
    printf("%d ", sorted[i]);
  }
  printf("\n");
  clean_up(sorted);
}

int main(int argc, char *argv[]) {
  if (argc < 2) die("USAGE: ex18 [1 2 3 4]");

  int count = argc - 1;
  int i = 0;
  char **inputs = argv + 1;

  int *numbers = malloc(count * sizeof(int));
  if (!numbers) die("Error allocating for arguments");

  for (i = 0; i < count; i++) {
    numbers[i] = atoi(inputs[i]);
  }

  test_sorting(numbers, count, ascending);
  test_sorting(numbers, count, descending);
  test_sorting(numbers, count, strange_order);

  clean_up(numbers);

  return 0;
}
