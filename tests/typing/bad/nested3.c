#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

int main() {
  int x;
  {
    int x() { return 0; }
    x = 1;
  }
}
