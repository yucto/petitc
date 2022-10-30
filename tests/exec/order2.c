
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

void f(int n, int m, int k) {
  putchar(n);
  putchar(m);
  putchar(k);
  putchar(10);
  if (n != 'c') f(m, k, n);
}

int main() {
  f('a', 'b', 'c');
}
