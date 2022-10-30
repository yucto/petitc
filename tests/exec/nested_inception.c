
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

int main() {
  int y = 'Y';
  int a = 'A';
  int e = 'E';
  int c = 'C';
  int s = 'S';
  int a2 = 'A';
  int r = 'R';
  int k = 1;
  bool yb = true;
  bool ab = true;
  bool eb = true;
  bool cb = true;
  bool sb = true;
  bool a2b = true;
  bool rb = true;
  void present(bool b, int c) {
    putchar(' ');
    if (b) putchar(c); else putchar(' ');
  }
  void draw() {
    for (int i = 0; i < k; i++) {
      present(yb, y);
      present(ab, a);
      present(eb, e);
      present(a2b, a2);
      present(rb, r);
      present(cb, c);
      present(sb, s);
      putchar(10);
    }
  }
  void airplane() {
    void van() {
      void hotel() {
        void fortress() {
          void dream_city() {
            void limbo() {
              draw();
            }
            for (int i = 0; i < 14; i++) putchar('-');
            putchar(10);
            a2b = false;
            rb = false;
            limbo();
          }
          draw();
          dream_city();
        }
        draw();
        eb = false;
        fortress();
      }
      draw();
      ab = false;
      hotel();
    }
    draw();
    yb = false;
    van();
  }
  airplane();
}
