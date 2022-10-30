
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

int main() {
  int *s;
  s = malloc(4);
  s[0] = 'h';
  putchar(s[0]);
  putchar(10);
}
