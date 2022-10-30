
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

/* arithmetique de virgule fixe
   precision q = 8192 i.e. 13 bits pour la partie decimale */

int add(int x, int y) {
  return x + y;
}
int sub(int x, int y) {
  return x - y;
}
int mul(int x, int y) {
  int t;
  t = x * y;
  return (t + 8192 / 2) / 8192;
}
int divide(int x, int y) {
  int t;
  t = x * 8192;
  return (t + y / 2) / y;
}
int of_int(int x) {
  return x * 8192;
}

int inside(int x, int y) {
  int iter(int n, int xn, int yn) {
    if (n == 100) return 1;
    int xn2 = mul(xn, xn);
    int yn2 = mul(yn, yn);
    if (add(xn2, yn2) > of_int(4)) return 0;
    return iter(n+1,
                add(sub(xn2, yn2), x),
                add(mul(of_int(2), mul(xn, yn)), y));
  }
  return iter(0, of_int(0), of_int(0));
}

void run(int steps) {
  int xmin = of_int(-2);
  int xmax = of_int(1);
  int deltax = divide(sub(xmax, xmin), of_int(2 * steps));
  int ymin = of_int(-1);
  int ymax = of_int(1);
  int deltay = divide(sub(ymax, ymin), of_int(steps));
  for (int i = 0; i < steps; i++) {
    int y = add(ymin, mul(of_int(i), deltay));
    for (int j = 0; j < 2 * steps; j++) {
      int x = add(xmin, mul(of_int(j), deltax));
      if (inside(x, y))
        putchar('0');
      else
        putchar('1');
    }
    putchar(10);
  }
}

int main() {
  run(30);
}
