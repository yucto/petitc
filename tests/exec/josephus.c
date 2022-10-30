
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

int* cycle(int n) {
  int *c = malloc(n * sizeof(int));
  for (int i = 0; i < n; i++) c[i] = (i+1) % n;
  return c;
}

int josephus(int n, int p) {
  int *c = cycle(n);
  int i = 0;
  while (c[i] != i) {
    /* on élimine un joueur */
    for (int j = 1; j < p-1; j++) i = c[i];
    c[i] = c[c[i]];
    i = c[i];
  }
  return i+1;
}

void print_int(int n) {
  if (n > 9) print_int(n / 10);
  putchar('0' + n%10);
}

int main() {
  print_int(josephus(7, 5)); // 6
  putchar(10);
  print_int(josephus(5, 5)); // 2
  putchar(10);
  print_int(josephus(5, 17)); // 4
  putchar(10);
  print_int(josephus(13, 2)); // 11
  putchar(10);
}
