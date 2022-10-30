
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

void f(int n, int m) {
  putchar(n);
  putchar(m);
  putchar(10);
}

int main() {
  f('a', 'b');
}
