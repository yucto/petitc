
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

void f(int x, int y) {
  putchar(x);
}

int main() {
  f('A', 'B');
  f('B', 'A');
  putchar(10);
}
