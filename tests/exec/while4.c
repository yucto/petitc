
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

int main() {
  int x;
  x = 10;
  while(x) putchar('A' + --x);
  putchar(10);
}
