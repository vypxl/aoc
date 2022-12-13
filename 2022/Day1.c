#include <stdio.h>
#include <time.h>

void solve() {
  FILE* f = fopen("1.in", "r");
  int s = 0;
  int x;
  int max1, max2, max3;
  max1 = max2 = max3 = 0;

  while (1) {
    if ((x = fgetc(f)) == '\n') {
      s = 0;
    } else {
      fseek(f, -1, SEEK_CUR);
      fscanf(f, "%d", &x);
      if (x == -1) break;
      s += x;
      fseek(f, 1, SEEK_CUR);
    }

    if (s > max1) {
      max3 = max2;
      max2 = max1;
      max1 = s;
    } else if (s > max2) {
      max3 = max2;
      max2 = s;
    } else if (s > max3) {
      max3 = s;
    }
  }

  printf("Solution for part 1: %d\n", max1);
  printf("Solution for part 2: %d\n", max1 + max2 + max3);
  fclose(f);
}

int main (int argc, char **argv) {
  double t = 0.0;
  for (int i = 0; i < 10000; ++i) {
    double startTime = (double)clock()/CLOCKS_PER_SEC;

    solve();

    double endTime = (double)clock()/CLOCKS_PER_SEC;

    double timeElapsed = endTime - startTime;
    t += timeElapsed;
  }
  t /= 100000;
  printf("Took %f ms\n", t * 1000);

  return 0;
}

// Solution part 1: 67450
// Solution part 2: 199357
