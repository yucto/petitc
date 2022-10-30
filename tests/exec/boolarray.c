
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

int main() {
  bool *p = malloc(2 * sizeof(bool));
  p[0] = true;
  p[1] = false;
  putchar('A' + *p++);
  putchar('A' + *p++);
}
