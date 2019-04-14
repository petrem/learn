#include "secret_handshake.h"

#include <stdbool.h>
#include <stdlib.h>
#include <errno.h>

#include <stdio.h>


static void *calloc_or_die(size_t nmemb, size_t size) {
  void *ptr = calloc(nmemb, size);
  if (ptr == NULL)
    exit(ENOMEM);
  return ptr;
}


/* was cmd command given? */
static inline bool is_cmd(unsigned int commands, unsigned int cmd) {
  return commands & cmd;
}


/* calculate the number of codes we'll give */
static inline unsigned int count_codes(unsigned int commands) {
  return (
          is_cmd(commands, CMD_WINK) +
          is_cmd(commands, CMD_DOUBLE_BLINK) +
          is_cmd(commands, CMD_CLOSE_EYES) +
          is_cmd(commands, CMD_JUMP));
}


/* first element in codes list is 0, or last if the reverse command is active */
static inline unsigned int first(unsigned int commands) {
  return is_cmd(commands, CMD_REVERSE) ? count_codes(commands) - 1 : 0;
}


/* increase the index to codes, or decrese if the reverse command is active */
static inline unsigned int next(unsigned int commands, unsigned int i) {
  return is_cmd(commands, CMD_REVERSE)? i - 1 : i + 1;
}


const char **commands(unsigned int commands) {
  static const char *all_codes[] = {"wink", "double blink", "close your eyes", "jump"};
  /* allocate as many codes as needed, but at least one */
  const char **codes = calloc_or_die(count_codes(commands) ? count_codes(commands) : 1, sizeof(char *));
  unsigned int cursor = 1;

  for (unsigned int i = 0, j = first(commands); i < 4; i++) {
    if (is_cmd(commands, cursor)) {
      codes[j] = all_codes[i];
      j = next(commands, j);
    }
    cursor <<= 1;
  }

  return codes;
}
