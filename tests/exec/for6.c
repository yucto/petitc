
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

int main() {
  for (int x = 10; x--; x--) putchar('A' + x);
  putchar(10);
}
