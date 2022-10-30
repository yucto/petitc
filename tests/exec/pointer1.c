
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

int main() {
  int *p;
  p = malloc(5 * sizeof(int));
  *p = 1000;
  p++;
  *p = 2000;
  p++;
  *p = 3000;
  if (p[-2] == 1000) putchar('A');
  if (p[-1] == 2000) putchar('B');
  if (p[0] == 3000) putchar('C');
  putchar(10);
}
