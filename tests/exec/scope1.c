
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

int main() {
  int x = 10;
  for (int x = 65; x < 91; x++) {
    putchar(x);
    int x = '.';
    putchar(x);
  }
  putchar(x);
}
