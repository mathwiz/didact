#include <stdio.h>

#define SAFE_RAD 0.466
#define SAFETY_FACT 10.0

int rad_table(double init_radiation, double min_radiation);


int main(void)
{
  int day = 0;
  double init_radiation, min_radiation;

  min_radiation = SAFE_RAD / SAFETY_FACT;

  printf("Enter the radiation level (in millirems)> ");
  scanf("%lf", &init_radiation);

  day = rad_table(init_radiation, min_radiation);

  printf("\nYou can enter the room on day %d.\n", day);

  return 0;
}


int rad_table(double init_radiation, double min_radiation) {
  int day = 0;
  double radiation_lev;

  printf("\n      DAY    RADIATION    STATUS ");
  printf("\n            (millirems)\n ");

  for(radiation_lev = init_radiation; radiation_lev > min_radiation; radiation_lev /= 2.0) {
     
     if (radiation_lev > SAFE_RAD) {
       printf("    %3d%5c%9.4f    Unsafe\n ", day, ' ', radiation_lev);
     } else {
       printf("    %3d%5c%9.4f    Safe\n ", day, ' ', radiation_lev);
     }

     day += 3;
  }

  return day;
}
