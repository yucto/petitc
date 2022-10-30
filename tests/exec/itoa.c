
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

int* itoa(int n) {
  int len;
  int abs;
  int *s;
  len = 1;
  if (n < 0) { abs = -n; len++; } else abs = n;
  abs = abs / 10;
  while (abs != 0) { len++; abs = abs / 10; }
  s = malloc((len + 1) * sizeof(int));
  s[len--] = 0;
  if (n < 0) { s[0] = '-'; n = -n; };
  while (n > 9) { s[len--] = '0' + n % 10; n = n / 10; }
  s[len] = '0' + n % 10;
  return s;
}

void print_string(int *s) {
  int c;
  while (c = *s++) putchar(c);
}

void print_endline(int *s) {
  print_string(s);
  putchar(10);
}

int main() {
  print_endline(itoa(0));
  print_endline(itoa(17));
  print_endline(itoa(5003));
  print_endline(itoa(-12));
  return 0;
}
