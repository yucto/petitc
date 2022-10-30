
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

// tri par tas

void heapsort(int *a, int n) {
  void move_down(int i, int x, int n) {
    while (1) {
      int j = 2 * i + 1;
      if (j >= n) break;
      if (j + 1 < n && a[j] < a[j + 1]) { j++; }
      if (a[j] <= x) break;
      a[i] = a[j];
      i = j;
    }
    a[i] = x;
  }
  for (int k = n / 2 - 1; k >= 0; k--)
    move_down(k, a[k], n);
  for (int k = n - 1; k >= 1; k--) {
    int v = a[k];
    a[k] = a[0];
    move_down(0, v, k);
  }
}

int main() {
  int n = 5;
  int *a = malloc(n * sizeof(int));
  a[0] = 'd';
  a[1] = 'e';
  a[2] = 'a';
  a[3] = 'b';
  a[4] = 'c';
  heapsort(a, n);
  for (int i = 0; i < n; i++)
    putchar(a[i]);
  putchar('\n');
}
