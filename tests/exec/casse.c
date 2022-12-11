#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

int main() {
  int *x = malloc(2*8);
  x[0] = 'a';
  x[1] = 'b';
  putchar((*(x++))++);
  putchar('\n');
}
