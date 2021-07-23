#include <stdio.h>
#include <math.h>

#define G 32.17 /* gravitational constant */

int main(void)
{
    /* inputs */
    double theta;       /* angle (radians) of elevation */
    double distance;    /* distance (ft) to vertical target */
    double velocity;    /* projectile speed (ft/sec) */
    
    /* outputs */
    double time;        /* time (sec) airborne */
    double height;      /* height (ft) at impact */
    
    printf("Enter the angle of elevation in radians> ");
    scanf("%lf", &theta);
    
    printf("Enter the distance to the vertical target in feet> ");
    scanf("%lf", &distance);

    printf("Enter the projectile speed in ft/sec> ");
    scanf("%lf", &velocity);
    
    time = distance / (velocity * cos(theta));
    height = velocity * sin(theta) * time - G * pow(time, 2) / 2;
    
    printf("Impact occurred at %.2f seconds %.2f feet above the ground.", time, height);
    printf("\n");

    return 0;
}
