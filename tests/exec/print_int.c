
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

void print_int(int n) {
  if (n > 9) print_int(n / 10);
  putchar('0' + n%10);
}

int main() {
  print_int(42);
  putchar(10);
}
