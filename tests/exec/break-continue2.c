
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

void print2(int n) {
  if (n < 10) putchar(' '); else putchar('0' + n/10);
  putchar('0' + n % 10);
}

int main() {
  for (int i = 0; i < 10; i++) {
    if (i == 0) continue;
    putchar('|');
    for (int j = 0; j < 10; j++) {
      if (j == 0) continue;
      print2(i * j);
      if (j == 9) break;
      putchar('|');
    }
    putchar('|');
    putchar(10);
  }
}
