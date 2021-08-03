#include <stdio.h>
#define KMS_PER_MILE 1.609

int main(void)
{
    double miles, kms;
    
    /* input from stdin */
    scanf("%lf", &miles);
    printf("%.4f\n", KMS_PER_MILE * miles);
    
    return 0;
}
