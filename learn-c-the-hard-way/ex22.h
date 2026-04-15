#ifdef _ex22_h
#define _ex22_h

// make avail to other .c files
extern int THE_SIZE;

// get and set an internal static variable in ex22.c file
int get_age();
void set_age(int age);

// updates static variable inside update_ratio
double update_ratio(double ratio);
void print_size();

#endif
