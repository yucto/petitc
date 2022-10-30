
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

int main() {
  int x = 10;
  for (; x; ) putchar('A' + --x);
  putchar(10);
}
