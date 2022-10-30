
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

void print_int(int n) {
  if (n > 9) print_int(n / 10);
  putchar('0' + n%10);
}

void compute(int n, int a, int b, int c, int d) {
  if (n == 0) return;
  int x = a+c;
  int y = b+d;
  compute(n-1, a, b, x, y);
  putchar(' '); print_int(x); putchar('/'); print_int(y);
  compute(n-1, x, y, c, d);
}

void stern_brocot(int n) {
  putchar('0'); putchar('/'); putchar('1');
  compute(n, 0, 1, 1, 1);
  putchar(' '); putchar('1'); putchar('/'); putchar('1'); putchar(10);
}

int main() {
  stern_brocot(0);
  stern_brocot(1);
  stern_brocot(2);
  stern_brocot(3);
  stern_brocot(4);
  stern_brocot(10);
}
