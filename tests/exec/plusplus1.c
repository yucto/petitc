
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

int main() {
  int x;
  x = 65;
  putchar(x);
  putchar(x++);
  putchar(x);
  putchar(x++);
  putchar(x);
  putchar(++x);
  putchar(x);
  putchar(++x);
  putchar(10);
}
