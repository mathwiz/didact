#include <stdio.h>

#define DEMAND_CHG  35.00
#define PER_1000_CHG  1.10
#define LATE_CHG  2.00
#define OVERUSE_CHG_RATE 2.00
#define CONSERVE_RATE 95

void instruct_water(void);
double comp_use_charge(int previous, int current, int last_year);
double comp_late_charge(double unpaid);
void display_bill(double late_charge, double bill, double unpaid);
void display_overuse_msg(int used, int use_last_year);

int main(int argc, const char * argv[])
{
  int previous;      /* input */
  int current;       /* input */
  int use_last_year; /* input */
  double unpaid;     /* input */
  double bill;       /* output */
  int used;
  double use_charge;
  double late_charge;

  /* user instructions */
  instruct_water();

  /* get input */
  printf("Enter unpaid balance> $");
  scanf("%lf", &unpaid);
  printf("Enter previous meter reading> ");
  scanf("%d", &previous);
  printf("Enter current meter reading> ");
  scanf("%d", &current);
  printf("Enter last year usage> ");
  scanf("%d", &use_last_year);

  use_charge = comp_use_charge(previous, current, use_last_year);

  late_charge = comp_late_charge(unpaid);

  bill = DEMAND_CHG + use_charge + unpaid + late_charge;

  display_bill(late_charge, bill, unpaid);

  return 0;
}

double comp_use_charge(int previous, int current, int use_last_year)
{
  int used = current - previous;
  double use_charge;
  if (used <= CONSERVE_RATE / 100.0 * use_last_year)
    use_charge = used * PER_1000_CHG;
  else
  {
    display_overuse_msg(used, use_last_year);
    use_charge = used * OVERUSE_CHG_RATE * PER_1000_CHG;
  }
  return use_charge;
}

void display_overuse_msg(int used, int use_last_year)
{
  printf("\n");
  printf("Use charge is at %.2f times ", OVERUSE_CHG_RATE);
  printf("normal rate since use of \n");
  printf("%d units exceeds %d percent ", used, CONSERVE_RATE);
  printf("of last year's %d-unit use.\n ", use_last_year);
}

double comp_late_charge(double unpaid)
{
  double late_charge = 0.0;
  if (unpaid > 0)
    late_charge = LATE_CHG;
 
  return late_charge;
}

void display_bill(double late_charge, double bill, double unpaid)
{
  if (late_charge > 0.0) {
    printf("\nBill includes $%.2f late charge ", late_charge);
    printf("on unpaid balance of $%.2f\n", unpaid);
  }
  printf("\nTotal due = $%.2f\n", bill);
}

void instruct_water(void)
{
  printf("This program figures a water bill ");
  printf("based on the demand charge\n");
  printf("($%.2f) and a $%.2f per 1000 ", DEMAND_CHG, PER_1000_CHG);
  printf("gallons use charge.\n\n");
  printf("A $%.2f surcharge is added to ", LATE_CHG);
  printf("accounts with an unpaid balance.\n");
  printf("\nEnter unpaid balance, previous ");
  printf("and current meter readings, \n");
  printf("and use last year\n");
  printf("on separate lines after the prompts.\n");
  printf("Press <ret> or <enter> after ");
  printf("typing each number.\n\n");
}
