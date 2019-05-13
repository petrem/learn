#include "binary_search.h"
#include <stddef.h>

int *binary_search(int needle, int haystack[], size_t length) {
  if (haystack == NULL || length == 0) {
    return NULL;
  }
  size_t left=0, right = length - 1;
  while (left < right) {
    size_t middle = left + ((right - left) >> 1);
    if (needle < haystack[middle]) {
      if (middle == 0) {
        break;
      }
      right = middle - 1;
    }
    else if (needle > haystack[middle]) {
      left = middle + 1;
    }
    else {
      return &haystack[middle];
    }
  }
  if (left == right && needle == haystack[left]) {
    return &haystack[left];
  }
  return NULL;
}
