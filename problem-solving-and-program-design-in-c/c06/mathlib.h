#include <stdlib.h>

int find_gcd(int n1, int n2);

int 
find_gcd (int u, int v) {
  if (u < 0) u = -u;
  if (v < 0) v = -v;
  if (v) while ((u %= v) && (v %= u));
  return (u + v);
}
