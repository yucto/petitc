
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

int main() {
  int x;
  int y;
  x = 65;
  putchar(x);
  putchar(x = x+1);
  putchar(y = x+1);
  putchar(10);
}
