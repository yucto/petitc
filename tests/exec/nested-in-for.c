
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

int main() {
  for (int x = 65; x < 91; x++) {
    void f() { putchar(x); }
    f();
  }
  putchar(10);
}
