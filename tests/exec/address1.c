
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

int main() {
  int x;
  int *p;
  p = &x;
  *p = 'A';
  putchar(x);
  putchar(*p);
  x = 'B';
  putchar(x);
  putchar(*p);
  *p = 'C';
  putchar(x);
  putchar(*p);
  putchar(10);
}
