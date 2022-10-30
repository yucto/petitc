
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

int main() {
  int f(int a1) {
    int a2 = 1;
    int g(int b1, int b2) {
      int b3 = 3;
      int b4 = 5;
      int h(int c1, int c2, int c3) {
        int c4 = 34;
        return a1 + a2 + b1 + b2 + b3 + b4 +
               c1 + c2 + c3 + c4 + 1;
      }
      return h(8, 13, 21);
    }
    return g(1, 2);
  }
  putchar(f(0));
  putchar('e');
  putchar('s');
  putchar(10);
}
