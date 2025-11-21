#include <stdio.h>

typedef enum
{ monday, tuesday, wednesday, thursday, friday, saturday, sunday }
day_t;

void print_day(day_t day);

int
main(void) {
  day_t today;
  double week_hours = 0.0, day_hours;
  for (today = monday; today <= friday; ++today) {
    printf("Enter hours for ");
    print_day(today);
    printf("> ");
    scanf("%lf", &day_hours);
    week_hours += day_hours;
  }

  printf("\nTotal weekly hours are %.2f\n", week_hours);

  return 0;
}

void
print_day(day_t day_kind) {
  switch (day_kind)
    {
      case monday:
        printf("Monday");
        break;
      case tuesday:
        printf("Tuesday");
        break;
      case wednesday:
        printf("Wednesday");
        break;
      case thursday:
        printf("Thurday");
        break;
      case friday:
        printf("Friday");
        break;
      case saturday:
        printf("Saturday");
        break;
      case sunday:
        printf("Sunday");
        break;

      default:
        printf("*** INVALID DAY ***");
    }
}
