/*
 * Draw a stick figure
 */

#include <stdio.h>

/* functions */

void draw_spacer(void);
void draw_circle(void);
void draw_intersect(void);
void draw_base(void);
void draw_triangle(void);

int
main(void)
{
  draw_circle();
  draw_spacer();
  draw_triangle();
  draw_spacer();
  draw_intersect();
  draw_spacer();

  return 0;
}


void
draw_spacer(void)
{
  printf("\n");
}


void
draw_circle(void)
{
  printf("  *\n");
  printf("*   *\n");
  printf(" * *\n");
}


void
draw_intersect(void)
{
  printf("   /\\\n");
  printf("  /  \\\n");
  printf(" /    \\\n");
  printf("/      \\\n");
}


void
draw_base(void)
{
  printf("--------\n");
}


void
draw_triangle(void)
{
  draw_intersect();
  draw_base();
}
