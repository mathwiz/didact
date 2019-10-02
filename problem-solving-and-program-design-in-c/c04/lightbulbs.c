#include <stdio.h>
#include <stdlib.h>

int brightness(int watts) 
{
    int lumens = 0;
    switch (watts) {
    
    case 15:
    lumens = 125;
    break;
    
    case 25:
    lumens = 215;
    break;
    
    case 40:
    lumens = 500;
    break;
    
    case 60:
    lumens = 880;
    break;
    
    case 75:
    lumens = 1000;
    break;
    
    case 100:
    lumens = 1675;
    break;
    
    default:
    lumens = -1;
    }
    
    return lumens;
}

int main(int argc, const char * argv[])
{
    int watts = 0;
    int lumens;

    if (argc == 1) {
        printf("Usage: programname watts.\n");
        return -1;
    } else if (argc == 2) {
        watts = atoi(argv[1]);
    }
    
    lumens = brightness(watts);
    
    if (lumens == -1) {
        printf("%d is an unknown wattage.", watts);
    } else {
        printf("The brightness of a %d watt lightbulb is %d lumens.", watts, lumens);
    }
    printf("\n");
    
    return 0;
}