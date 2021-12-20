#include <stdio.h>

#define CAPACITY 80000.0
#define MIN_PCT 10
#define GALS_PER_BRL 42.0

double monitor_gas(double min_supply, double start_supply);


int main(int argc, const char * argv[])
{
  double start_supply, min_supply, current;

  min_supply = MIN_PCT / 100.0 * CAPACITY;

  printf("Total tank capacity (barrels): %.2f \n", CAPACITY);
  printf("Number of barrels currently in tank> ");
  scanf("%lf", &start_supply);

  current = monitor_gas(min_supply, start_supply);

  printf("only %.2f barrels are left. \n\n", current);
  printf("*** WARNING ***\n");
  printf("Available supply is less than %d percent of tank's\n", MIN_PCT);
  printf("%.2f-barrel capacity.\n", CAPACITY);

  return 0;
}

double monitor_gas(double min_supply, double start_supply)
{
  double remove_gals, remove_brls, current;

  for (current = start_supply;
       current >= min_supply;
       current -= remove_brls) {
    printf("%.2f barrels are available.\n\n", current);
    printf("Enter number of gallons removed> ");
    scanf("%lf", &remove_gals);
    remove_brls = remove_gals / GALS_PER_BRL;
    printf("After removal of %.2f gallons (%.2f barrels), \n", 
           remove_gals, remove_brls);
  }
  return current;
}
