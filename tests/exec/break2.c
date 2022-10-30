
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

int main() {
  for (;;) {
    putchar('A');
    break;
    putchar('C');
  }
  putchar('B');
  putchar(10);
}
