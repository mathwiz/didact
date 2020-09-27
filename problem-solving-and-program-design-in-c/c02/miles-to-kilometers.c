#include <stdio.h>
#define KMS_PER_MILE 1.609

int main(void)
{
    double miles, kms;
    
    printf("Enter the distance in miles> ");
    scanf("%lf", &miles);
    
    kms = KMS_PER_MILE * miles;
    
    printf("That equals %.2f kilometers.\n", kms); /* note the use of
                                                      decimal format */
    
    return 0;
}
