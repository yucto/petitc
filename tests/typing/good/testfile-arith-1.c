#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
int main() {
  int *p;
  putchar((p+1) - p);
}

