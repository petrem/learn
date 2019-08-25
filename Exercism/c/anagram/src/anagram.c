#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <ctype.h>
#include <stdio.h>

#include "anagram.h"

static enum anagram_status
is_anagram(const char *word, const unsigned * wordsignature, const char *candidate);
static char *strtolower(const char *word);
static unsigned *signature(const char *word);
static bool compare_signatures(const unsigned *sig1, const unsigned *sig2);


void anagrams_for(const char *word, struct candidates *candidates) {
  char *lowword = strtolower(word);
  unsigned *word_signature = signature(lowword);
  for (size_t i = 0; i < candidates->count; i++) {
    struct candidate *candidate = &candidates->candidate[i];
    candidate->is_anagram = is_anagram(lowword, word_signature, candidate->candidate);
    //printf("%s: %d\n", candidate->candidate, candidate->is_anagram);
  }
  free(lowword);
  free(word_signature);
}

static enum anagram_status
is_anagram(const char *word, const unsigned *word_signature, const char *candidate) {
  char * lowcandidate = strtolower(candidate);
  enum anagram_status status = IS_ANAGRAM;
  if (strcmp(word, lowcandidate) == 0)
    status = NOT_ANAGRAM;
  else {
    unsigned *candidate_signature = signature(lowcandidate);
    if (!compare_signatures(word_signature, candidate_signature))
      status = NOT_ANAGRAM;
    free(candidate_signature);
  }
  free(lowcandidate);
  return status;
}

static unsigned *signature(const char *word) {
  unsigned *sig = calloc(26, sizeof(unsigned));
  unsigned i = 0;
  while (word[i]) {
    sig[word[i++] - 'a']++;
  }
  return sig;
}

static bool compare_signatures(const unsigned *sig1, const unsigned *sig2) {
  for (unsigned i = 0; i < 26 ; i++)
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
