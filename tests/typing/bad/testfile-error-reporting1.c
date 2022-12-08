#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

void f(void *x, void *y) {}

int main() {
  int x;
  x = NULL;

  int y = NULL;
  f(x, y);
  f();
}
