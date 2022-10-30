
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

void f(int n) {
  void g(int m) {
    void add(int *x) { *x = *x + m; }
    add(&n);
    putchar(n);
  }
  g('h' - 'a');
  g('e' - 'h');
  g('l' - 'e');
  g('l' - 'l');
  g('o' - 'l');
  g(' ' - 'o');
  g('w' - ' ');
  g('o' - 'w');
  g('r' - 'o');
  g('l' - 'r');
  g('d' - 'l');
  g('\n'- 'd');
}

int main() {
  f('a');
}
