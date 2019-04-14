#include "grains.h"
#include <stdint.h>

uint64_t square(unsigned int n) {
  if (n == 0 || ((n - 1) & 0x40) != 0) {
    return 0;
  }
  return 1ull << (n - 1);
}

uint64_t total() {
  uint64_t total = 0;
  for(unsigned int i = 1; i <= 64; i++) {
    total |= square(i);
  }
  return total;
}
