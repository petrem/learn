#include "difference_of_squares.h"
#include <stdio.h>

long square_of_sum(long n) {
  long sum = 0;
  for (; n>0; n--) {
    sum += n;
  }
  return sum * sum;
}

long sum_of_squares(long n) {
  long sum = 0;
  for (; n>0; n--) {
    sum += n*n;
  }
  return sum;
}


long difference_of_squares(long n) {
  long sum = 0;
  long half_diff = 0;
  for (long i = 1; i < n; i++) {
    sum += i;
    half_diff += sum * (i+1);
  }
  return 2 * half_diff;
}
