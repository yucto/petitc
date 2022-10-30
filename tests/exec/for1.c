
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

int main() {
  int cpt = 0;
  for(int i = 0; i < 10; i++) {
    for (int j = 10; j > 0; j--)
      ++cpt;
  }
  if (cpt == 100)
    putchar('!');
  putchar(10);
}
