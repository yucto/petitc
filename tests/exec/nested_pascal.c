
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

/* triangle de Pascal modulo 7 */

void pascal(int n) {
  int *r = malloc((n+1) * sizeof(int));
  void print_row(int i) {
    for (int j = 0; j <= i; j++)
      if (r[j] != 0)
        putchar('*');
      else
        putchar('.');
    putchar(10);
  }
  void compute_row(int i) {
    r[i] = 0;
    for (int j = i; j > 0; j--)
      r[j] = (r[j] + r[j-1]) % 7;
    r[0] = 1;
  }
  for (int i = 0; i < n; i++) {
    compute_row(i);
    print_row(i);
  }
}

int main() {
  pascal(42);
}
