#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

void f(int *x) {}
int main() { bool *p; f(p); }

