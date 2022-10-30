
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

int main() {
  int x;
  x = 65;
  putchar(x);
  if (x) x = 66;
  putchar(x);
  if (x && 0) x = 67;
  putchar(x);
  if (x && 1) x = 68;
  putchar(x);
  if (x || 0) x = 69;
  putchar(x);
  if (x || 1) x = 70;
  putchar(x);
  putchar(10);
}
