/*
 * Computes the weight of a batch of flat washers
 */

#include <stdio.h>
#define PI 3.14159

int
main(void)
{
  /* inputs */
  double hole_diameter;
  double edge_diameter;
  double thickness;
  double density;
  double quantity;

  /* output */
  double weight;

  /* computations */
  double hole_radius;
  double edge_radius;
  double rim_area;
  double unit_weight;

  /* get inner, outer diameters and thickness */
  printf("Inner diameter in centimeters> ");
  scanf("%lf", &hole_diameter);
  printf("Outer diameter in centimeters> ");
  scanf("%lf", &edge_diameter);
  printf("Thickness in centimeters> ");
  scanf("%lf", &thickness);

  /* get the material density and quantity manufactured */
  printf("Material density in grams per cubic centimeter> ");
  scanf("%lf", &density);
  printf("Quantity in batch> ");
  scanf("%lf", &quantity);

  /* compute rim area */
  hole_radius = hole_diameter / 2.0;
  edge_radius = edge_diameter / 2.0;
  rim_area = PI * edge_radius * edge_radius - PI * hole_radius * hole_radius;

  /* compute weight */
  unit_weight = rim_area * thickness * density;

  /* compute weight of batch */
  weight = unit_weight * quantity;

  /* output */
  printf("\nThe expected weight of the batch is %.2f grams.\n", weight);

  return 0;
}
