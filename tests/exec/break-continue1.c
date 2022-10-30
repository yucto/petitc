
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

int main() {
  for (int n = 65; n < 91; n++) {
    putchar(n);
    if (n % 2 == 0) continue;
  }
  putchar(10);
  for (int n = 65; n < 91; n++) {
    if (n % 2 == 0) continue;
    putchar(n);
  }
  putchar(10);
  for (int n = 65; n < 91; n++) {
    if (n % 2 == 0) continue;
    putchar(n);
    if (n > 'K') break;
  }
  putchar(10);
}
