
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

void f(int x, int y, int z, int t) {
  if (!x) return;
  putchar(x);
  f(y, z, t, x);
}

int main() {
  f('A', 'B', 'C', 0);
  putchar(10);
}
