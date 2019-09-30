#include <stdio.h>
#define SCALE 5.0 / 9
#define OFFSET 32

int main(void)
{
    double celsius, fahrenheit;
    
    /* input and output from files */
    FILE *inp, *outp;
    
    inp = fopen("temp.dat", "r");
    outp = fopen("temp.out", "w");
    
    fscanf(inp, "%lf", &fahrenheit);
    
    celsius = SCALE * (fahrenheit - OFFSET);
    fprintf(outp, "%.2f fahrenheit is %.2f celsius", fahrenheit, celsius);
    
    fclose(inp);
    fclose(outp);
    
    return 0;
}