#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

int main() {
  int f;
  { int f() { return 0; }
    f = 1;
  }
}
