
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

void print(void *p) {
  int *x = p;
  putchar(*x);
}

int main() {
  int x = 42;
  int *p = &x;
  void *v = p;
  print(v);
  putchar(10);
}
