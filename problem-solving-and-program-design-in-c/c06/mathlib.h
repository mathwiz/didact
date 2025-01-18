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
  int denom, numer, sign_factor;

  denom = d1 * d2;
  numer = n1 * d2 + n2 * d1;
  /* -1 for negative, 1 otherwise */
  sign_factor = (numer * denom < 0) ? -1 : 1;
  numer = sign_factor * abs(numer);
  denom = abs(denom);

  /* set result */
  *n_ansp = numer;
  *d_ansp = denom;
}

void 
multiply_fractions(int n1, int d1, int n2, int d2, int *n_ansp, int *d_ansp) {
  int denom, numer, sign_factor; 

  denom = d1 * d2;
  numer = n1 * n2;
  /* -1 for negative, 1 otherwise */
  sign_factor = (numer * denom < 0) ? -1 : 1;
  numer = sign_factor * abs(numer);
  denom = abs(denom);

  /* set result */
  *n_ansp = numer;
  *d_ansp = denom;
}

void 
reduce_fraction(int *np, int *dp) {
  int gcd;
  gcd = find_gcd(*np, *dp);
  *np = *np / gcd;
  *dp = *dp / gcd;
}
