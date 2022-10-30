
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

void f(int n) {
  int g(int m) { return n + m; }
  putchar(g(1));
}

int main() {
  f('a');
  putchar(10);
}
