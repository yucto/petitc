
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

// des listes d'entiers représentées dans un grand tableau mem,
// sans désallocation

void run(int n) {
  int *mem = malloc(n * sizeof(int));
  int null = -1;
  int next = 0;
  int car(int l) { return mem[l]; }
  void set_car(int l, int x) { mem[l] = x; }
  int cdr(int l) { return mem[l+1]; }
  void set_cdr(int l, int x) { mem[l+1] = x; }
  int cons(int car, int cdr) {
    int l = next;
    next = next + 2;
    mem[l] = car;
    mem[l+1] = cdr;
    return l;
  }
  // renvoie la liste lo,lo+1,...,hi-1
  int interval(int lo, int hi) {
    int l = null;
    while (lo < hi--) {
      l = cons(hi, l);
    }
    return l;
  }
  int list_reversal(int l) {
    int last = null;
    while (l != null) {
      int tmp = l;
      l = cdr(l);
      set_cdr(tmp, last);
      last = tmp;
    }
    return last;
  }
  void print(int l) {
    for ( ; l != null; l = cdr(l))
      putchar(car(l));
    putchar(10);
  }
  int l = interval('a', 'z' + 1);
  print(l);
  l = list_reversal(l);
  print(l);
}

int main() {
  run(100);
}
