#include <ctype.h>
#include <stdint.h>
#include <string.h>
#include "isogram.h"

bool is_isogram(const char phrase[]) {
  uint8_t occurences[26];
  char *c;
  int idx;

  if (phrase == NULL)
    return false;

  memset(occurences, 0, 26);
  for (c = (char *)phrase; *c; c++) {
    if (isalpha(*c)) {
      idx = tolower(*c) - 'a';
      if (occurences[idx] == 1)
        return false;
      occurences[idx] = 1;
    }
  }
  return true;
}
