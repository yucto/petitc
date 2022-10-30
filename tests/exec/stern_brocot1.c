
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

void print_int(int s, bool zero, int n) {
  if (s == 0) return;
  print_int(s - 1, false, n / 10);
  if (n == 0 && !zero) putchar(' '); else putchar('0' + n%10);
}

void print(int *num, int *den, int n, int d) {
  int s = 1 + d/5;
  for (int i = 0; i < n; i++) {
    putchar(' ');
    print_int(s, true, num[i]);
  }
  putchar('\n');
  for (int i = 0; i < n; i++) {
    putchar(' ');
    for (int j = 0; j < s; j++) putchar('-');
  }
  putchar('\n');
  for (int i = 0; i < n; i++) {
    putchar(' ');
    print_int(s, true, den[i]);
  }
}

int pow2(int n) {
  if (n == 0) return 1;
  int p = pow2(n / 2);
  p = p * p;
  if (n % 2 == 1) p = 2 * p;
  return p;
}

void compute(int *num, int *den, int d, int lo, int hi) {
  if (d == 0) return;
  int mid = lo + (hi - lo) / 2;
  num[mid] = num[lo] + num[hi];
  den[mid] = den[lo] + den[hi];
  compute(num, den, d-1, lo, mid);
  compute(num, den, d-1, mid, hi);
}

void stern_brocot(int d) {
  int n = pow2(d) + 1;
  int *num = malloc(n * sizeof(int));
  int *den = malloc(n * sizeof(int));
  num[0] = 0; den[0] = 1;
  num[n-1] = 1; den[n-1] = 1;
  compute(num, den, d, 0, n-1);
  print(num, den, n, d);
}

int main() {
  stern_brocot(4);
  putchar(10);
}
