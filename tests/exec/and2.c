
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

int main() {
  putchar(65 + (0 && 1));
  putchar(65 + (0 && 2));
  putchar(65 + (1 && 0));
  putchar(65 + (0 && 0));
  putchar(10);
}
