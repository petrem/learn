#include <stdint.h>
#include <stddef.h>
#include <string.h>


#define UNMARKED  (char)0
#define NOT_PRIME (char)1


uint32_t sieve(uint32_t limit, uint32_t *primes, size_t max_primes) {
  uint32_t n_primes = 0;
  char marks[limit + 1];
  uint32_t i = 2;
  memset(marks, UNMARKED, limit + 1);
  for (; i * i <= limit; i++) {
    if (marks[i] == UNMARKED) {
      primes[n_primes++] = i;
      if (n_primes == max_primes)
        break;
      for (uint32_t j = i * i; j <= limit ; j += i)
        marks[j] = NOT_PRIME;
    }
  }
  for (; i <= limit; i++) {
    if (marks[i] == UNMARKED) {
      primes[n_primes++] = i;
      if (n_primes == max_primes)
        break;
    }
  }
  return n_primes;
}
