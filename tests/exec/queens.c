
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

// les N reines

int abs(int x) {
  if (x < 0) return -x; else return x;
}

// la ligne k est-elle compatible avec les lignes précédentes ?
bool check(int n, int *sol, int k) {
  for (int i = 0; i < k; i++)
    if (sol[i] == sol[k] || abs(sol[i] - sol[k]) == abs(i - k))
      return false;
  return true;
}

// retour sur trace

// entrée : 0 <= k <= n et une solution partielle dans sol[0..k[
// sortie : true, si sol a pu être totalement complétée
//          false si ce n'est pas possible, et sol[0..k[ est inchangée
bool solve(int n, int *sol, int k) {
  if (k == n) return true;
  for (int v = 0; v < n; v++) {
    sol[k] = v;
    if (check(n, sol, k) && solve(n, sol, k+1))
      return true;
  }
  return false;
}

void print(int n, int *sol) {
  for (int i = 0; i < n; i++) {
    putchar('|');
    for (int j = 0; j < n; j++) {
      if (j == sol[i]) putchar('Q'); else putchar('.');
      putchar('|');
    }
    putchar(10);
  }
  putchar(10);
}

void queens(int n) {
  int *sol = malloc(n * sizeof(int));
  if (solve(n, sol, 0))
    print(n, sol);
}

int main() {
  for (int n = 1; n < 10; n++)
    queens(n);
}
