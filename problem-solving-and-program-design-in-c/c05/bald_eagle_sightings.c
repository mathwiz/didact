#include <stdio.h>

#define SENTINEL 0
#define NUM_MONTHS 12


int main(int argc, const char * argv[])
{
  int month, mem_sight, sightings;

  printf("BALD EAGLE SIGHTINGS\n\n");

  printf("Enter individual member sightings for the month (0 to end month).\n");

  for (month = 1; month <= NUM_MONTHS; ++month) {
    sightings = 0;
    scanf("%d", &mem_sight);
    while (mem_sight != SENTINEL) {
      if (mem_sight >= 0)
        sightings += mem_sight;
      else
        printf("Warning, negative count %d ignored\n", mem_sight);
      scanf("%d", &mem_sight);
    }
    printf("  month %2d: %2d\n", month, sightings);
  }  

  return 0;
}
