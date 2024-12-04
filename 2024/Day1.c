#include <stdio.h>
#define N 100000
int xs[N][2] = {0};

int main(int argc, char **argv) {
  FILE *inp = fopen("1.in", "r");

  int a, b;
  while (fscanf(inp, "%d   %d", &a, &b) && !feof(inp)) {
    ++xs[a][0];
    ++xs[b][1];
  }

  int i = 0, j = 0, s = 0, p2 = 0;

  while (i < N) {
    p2 += i * xs[i][0] * xs[i][1];
    ++i;
  }

  i = 0;

  while (i < N) {
    while (i < N && !xs[i][0])
      ++i;
    --xs[i][0];

    while (j < N && !xs[j][1])
      ++j;
    --xs[j][1];

    int x = i - j;
    int mask = x >> 31;
    s += (x + mask) ^ mask;
  }

  printf("Part1: %d\nPart2: %d\n", s, p2);

  return 0;
}
