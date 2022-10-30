
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

int main() {
  int x;
  x = 65;
  putchar(x);
  if (0) {
    int x;
    x = 66;
    putchar(x);
  } else {
    int x;
    int y;
    x = 67;
    y = 68;
    putchar(x);
    putchar(y);
  }
  putchar(x);
  putchar(10);
}
