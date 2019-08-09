#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>


#define WORDTYPE uint_fast32_t
#define WORDSIZE sizeof(WORDTYPE) * 8
#define WORDMASK (WORDTYPE) WORDSIZE - 1

#define UNMARKED  (unsigned) 0
#define NOT_PRIME (unsigned) 1

static WORDTYPE *bitvector_init(uint32_t limit);
static inline void bitvector_set(WORDTYPE *vector, uint32_t index, unsigned value);
static inline unsigned bitvector_get(WORDTYPE *vector, uint32_t index);


uint32_t sieve(uint32_t limit, uint32_t *primes, size_t max_primes) {
  size_t n_primes = 0;
  WORDTYPE *marks = bitvector_init(limit + 1);
  uint32_t i = 2;
  for (; i * i <= limit; i++) {
    if (bitvector_get(marks, i) == UNMARKED) {
      primes[n_primes++] = i;
      if (n_primes == max_primes)
        break;
      for (uint32_t j = i * i; j <= limit ; j += i)
        bitvector_set(marks, j, NOT_PRIME);
    }
  }
  for (; i <= limit; i++) {
    if (bitvector_get(marks, i) == UNMARKED) {
      primes[n_primes++] = i;
      if (n_primes == max_primes)
        break;
    }
  }
  return n_primes;
}


static WORDTYPE *bitvector_init(uint32_t limit) {
  WORDTYPE *data = calloc((limit + WORDSIZE - 1) / WORDSIZE, WORDSIZE);
  if (data == NULL) {
    fputs("Could not allocate memory. Good bye!", stderr);
    exit(ENOMEM);
  }
  return data;
}

static inline void bitvector_set(WORDTYPE *vector, uint32_t index, unsigned value) {
  size_t word_index = index / WORDSIZE;
  size_t shift = index & WORDMASK;
  WORDTYPE word = vector[word_index];
  if (((word >> shift) & 1) != value) {
    vector[word_index] = word ^ (1 << shift);
  }
}

static inline unsigned bitvector_get(WORDTYPE *vector, uint32_t index) {
  return (vector[index / WORDSIZE] >> (index & WORDMASK)) & 1;
}
