/*
* Calculate the area and circumference of a circle
*/

#include <stdio.h>
#define PI 3.14159

int main(void) {
  double radius;
  double area;
  double circum;

  /* get the radius */
  printf("Enter radius> ");
  scanf("%lf", &radius);
  
  area = PI * radius * radius;
  circum = 2 * PI * radius;

  printf("The area is %.4f\n", area);
  printf("The circumference is %.4f\n", circum);
    
  return 0;
}
