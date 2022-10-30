
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

void quicksort(int *a, int n) {
  void swap(int i, int j) {
    int tmp = a[i];
    a[i] = a[j];
    a[j] = tmp;
  }
  void quickrec(int l, int r) { // trie a[l..r[
    if (r - l <= 1) return;
    int p = a[l];
    int lo = l;
    int hi = r;
    for (int i = l+1; i < hi; ) {
      //    l      lo      i       hi     r
      //   +------+-------+-------+------+
      //   |  <p  |  =p   | ????? |  >p  |
      //   +------+-------+-------+------+
      if (a[i] < p) {
        swap(i++, lo++);
      } else if (a[i] == p) {
        i++;
      } else { // a[i] > p
        swap(i, --hi);
      }
    }
    if (lo - l < r - hi) {
      quickrec(l, lo);
      quickrec(hi, r);
    } else {
      quickrec(hi, r);
      quickrec(l, lo);
    }
  }
  // TODO mélanger le tableau a ici (si on disposait de rand())
  quickrec(0, n);
}

void print_int(int n) {
  if (n > 9) print_int(n / 10);
  putchar('0' + n % 10);
}

int main() {
  int n = 5;
  int *a = malloc(n * sizeof(int));
  a[0] = 42;
  a[1] = 21;
  a[2] = 42;
  a[3] = 1;
  a[4] = 1;
  quicksort(a, n);
  for (int i = 0; i < n; i++) {
    print_int(a[i]);
    putchar('\n');
  }
}

