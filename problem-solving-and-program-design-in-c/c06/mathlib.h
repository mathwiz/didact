#include <stdlib.h>

int find_gcd(int n1, int n2);
void add_fractions(int n1, int d1, int n2, int d2, int *n_ansp, int *d_ansp);
void multiply_fractions(int n1, int d1, int n2, int d2, int *n_ansp, int *d_ansp);
void reduce_fraction(int *np, int *dp);

int 
find_gcd (int u, int v) {
  if (u < 0) u = -u;
  if (v < 0) v = -v;
  if (v) while ((u %= v) && (v %= u));
  return (u + v);
}

void 
add_fractions(int n1, int d1, int n2, int d2, int *n_ansp, int *d_ansp) {
  printf("add_fractions");
}

void 
reduce_fraction(int *np, int *dp) {
  printf("reduce_fraction");
}
