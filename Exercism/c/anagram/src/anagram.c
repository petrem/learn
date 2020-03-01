#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <ctype.h>

#include "anagram.h"

static enum anagram_status
is_anagram(const char *word, const unsigned int *wordsignature, const char *candidate);
static char *strtolower(const char *word);
static unsigned int *signature(const char *word);
static bool compare_signatures(const unsigned int*sig1, const unsigned int *sig2);


void anagrams_for(const char *word, struct candidates *candidates) {
  char *lowword = strtolower(word);
  unsigned int *word_signature = signature(lowword);
  for (size_t i = 0; i < candidates->count; i++) {
    struct candidate *candidate = &candidates->candidate[i];
    candidate->is_anagram = is_anagram(lowword, word_signature, candidate->candidate);
  }
  free(lowword);
  free(word_signature);
}

static enum anagram_status
is_anagram(const char *word, const unsigned int *word_signature, const char *candidate) {
  char * lowcandidate = strtolower(candidate);
  enum anagram_status status = IS_ANAGRAM;
  if (strcmp(word, lowcandidate) == 0)
    status = NOT_ANAGRAM;
  else {
    unsigned int *candidate_signature = signature(lowcandidate);
    if (!compare_signatures(word_signature, candidate_signature))
      status = NOT_ANAGRAM;
    free(candidate_signature);
  }
  free(lowcandidate);
  return status;
}

static unsigned int *signature(const char *word) {
  unsigned int *sig = calloc(26, sizeof(unsigned int));
  unsigned int i = 0;
  while (word[i]) {
    sig[word[i++] - 'a']++;
  }
  return sig;
}

static bool compare_signatures(const unsigned int *sig1, const unsigned int *sig2) {
  for (unsigned int i = 0; i < 26 ; i++)
    if (sig1[i] != sig2[i])
      return false;
  return true;
}

static char *strtolower(const char *word) {
  char *dup = strndup(word, MAX_STR_LEN);
  size_t i = 0;
  while((dup[i] = tolower(dup[i]))) i++;
  return dup;
}


/* UTF-8 routines, taken from or inspired by https://www.cprogramming.com/tutorial/unicode.html */

