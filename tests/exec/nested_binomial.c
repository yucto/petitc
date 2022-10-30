
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

int comb1(int n, int k) {
  if (k == 0 || k == n) return 1;
  return comb1(n - 1, k - 1) + comb1(n - 1, k);
}

int comb2(int n, int k) {
  int **memo = malloc((n + 1) * sizeof(int*));
  for (int i = 0; i <= n; i++) {
    memo[i] = malloc((k + 1) * sizeof(int));
    for (int j = 0; j <= k ; j++)
      memo[i][j] = 0;
  }
  int compute(int n, int k) {
    int m = memo[n][k];
    if (m > 0) return m;
    if (k == 0 || k == n) m = 1;
    else m = compute(n - 1, k - 1) + compute(n - 1, k);
    return memo[n][k] = m;
  }
  return compute(n, k);
}

void print_int(int n) {
  if (n > 9) print_int(n / 10);
  putchar('0' + n%10);
}

int main() {
  print_int(comb1(10, 5)); putchar(10);
  print_int(comb2(10, 5)); putchar(10);
}
