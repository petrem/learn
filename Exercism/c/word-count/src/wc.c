#include "word_count.h"
#include <stdio.h>

int main() {
  printf("=== MAIN ===\n");
  word_count_word_t words[20];
  return word_count("jolly 'olly' has a olly nice dolly's,   how jolly  ", words);
  //return word_count("one,two,thaee", words);
}
