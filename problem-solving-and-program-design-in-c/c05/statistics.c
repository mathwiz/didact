#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define DEBUG 0

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
    double num = 0.0;
    int input_status;
    
    for (input_status = scanf("%lf", &num);
         input_status == 1;
         input_status = scanf("%lf", &num)) {
        if (DEBUG) printf("status: %d val: %f\n", input_status, num);
        calculate(num, &n, &sum, &sum_squares);
    }
    
    mean = calc_mean(n, sum);
    variance = calc_variance(n, sum_squares, mean);
    produce_output(n, sum, mean, variance, sqrt(variance));
    
    return 0;
}
