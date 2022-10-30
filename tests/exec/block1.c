
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

int main() {
  int x;
  x = 65;
  putchar(x);
  if (1) {
    int x;
    x = 66;
    putchar(x);
  } else {
    int x;
    x = 67;
    putchar(x);
  }
  putchar(x);
  putchar(10);
}
