#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define SENTINEL -1
#define DEBUG 0

double get_input()
{
    double number;
    printf("Enter a number (%d to stop) >> ", SENTINEL);
    scanf("%lf", &number);
    if (DEBUG) printf("Got input %f\n", number);
    return number;
}

void calculate(double number, int *count, double *sum, double *sum_squares)
{
    *count = *count + 1;
    *sum = *sum + number;
    *sum_squares = *sum_squares + (number * number);
}

double calc_mean(int n, double total)
{
    return total / n;
}

double calc_variance(int count, double ss, double mean)
{
    return (ss / count) - (mean * mean);
}

void produce_output(int n, double sum, double mean, double var, double sd)
{
    printf("%6s %16s %16s %16s %16s \n", "n", "sum", "mean", "variance", "std dev");
    printf("%6d %16.3f %16.3f %16.3f %16.3f \n", n, sum, mean, var, sd);
}

int main(int argc, const char * argv[])
{
    int n = 0;
    double sum = 0.0;
    double sum_squares = 0.0;
    double mean = 0.0;
    double variance = 0.0;
    double num = SENTINEL;
    
    num = get_input();
    for(; num != SENTINEL; ) {
        calculate(num, &n, &sum, &sum_squares);
        num = get_input();
    }
    
    mean = calc_mean(n, sum);
    variance = calc_variance(n, sum_squares, mean);
    produce_output(n, sum, mean, variance, sqrt(variance));
    
    return 0;
}
