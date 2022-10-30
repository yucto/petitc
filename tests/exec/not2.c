
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

int main() {
  int x;
  x = 0;
  putchar(65 + !x);
  x = 1;
  putchar(65 + !x);
  putchar(10);
}
