
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

int main() {
  int *p;
  p = malloc(5 * sizeof(int));
  p[0] = 1000;
  p[1] = 2000;
  p[2] = 3000;
  p[3] = 4000;
  p[4] = 5000;
  if (p[0] == 1000) putchar('A');
  if (p[1] == 2000) putchar('B');
  if (p[2] == 3000) putchar('C');
  putchar(10);
}
