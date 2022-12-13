#include <stdio.h>
#include <string.h>
#include <time.h>
#include <sys/mman.h>
#include <stdlib.h>
#include <errno.h>
#include <fcntl.h>

void solve_fread() {
  FILE* f = fopen("1.in", "r");
  char _buf[11000];
  // _buf[10998] = '\n';
  // _buf[10999] = 0;
  char* buf = _buf;
  fread(buf, sizeof(_buf), sizeof(_buf), f);
  fclose(f);

  char* start = buf;
  int s = 0;
  int x;
  int max1, max2, max3;
  max1 = max2 = max3 = 0;

  while (*buf != 0 && buf - start < 10998) {
    if (buf[0] == '\n') {
      s = 0;
      ++buf;
    } else {
      x = atoi(buf);
      s += x;
      while (*(buf++) != '\n');

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
  }

  printf("Solution for part 1: %d\n", max1);
  printf("Solution for part 2: %d\n", max1 + max2 + max3);
}

void solve_mmap() {
  int f = open("1.in", O_RDONLY);
  char* buf = mmap(NULL, 11000, PROT_READ, MAP_SHARED, f, 0);
  close(f);
  if (buf == MAP_FAILED) {
    printf("Map failed: %d\n", errno);
    return;
  }

  char* start = buf;
  int s = 0;
  int x;
  int max1, max2, max3;
  max1 = max2 = max3 = 0;
  int i = 0;

  while (*buf != 0 && buf - start < 10998) {
    if (buf[0] == '\n') {
      s = 0;
      ++buf;
    } else {
      x = atoi(buf);
      s += x;
      while (*(buf++) != '\n');

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
  }

  printf("Solution for part 1: %d\n", max1);
  printf("Solution for part 2: %d\n", max1 + max2 + max3);
  munmap(buf, 11000);
}

#define ITERS 10000

int main (int argc, char **argv) {
  double t = 0.0;
  for (int i = 0; i < ITERS; ++i) {
    double startTime = (double)clock()/CLOCKS_PER_SEC;

    // solve_mmap();
    solve_fread();

    double endTime = (double)clock()/CLOCKS_PER_SEC;

    double timeElapsed = endTime - startTime;
    t += timeElapsed;
  }
  t /= ITERS;
  printf("Took %f ms\n", t * 1000);

  return 0;
}

// Solution part 1: 67450
// Solution part 2: 199357
