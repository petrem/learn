#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>


void *calloc_or_die(size_t nmemb, size_t size) {
  void *ptr = calloc(nmemb, size);
  if (ptr == NULL)
    exit(ENOMEM);
  return ptr;
}

/* A binary heap storing NUL terminated strings */

struct bheap {
  int max;   /* available size */
  int len;    /* current number of elements */
  const char **heap; /* elements */
};

struct bheap *bheap_init(int max_elements) {
  struct bheap *bh = calloc_or_die(1, sizeof(struct bheap));
  bh->heap = calloc_or_die(max_elements, sizeof(char *));
  bh->max = max_elements;
  bh->len = 0;
  return bh;
}

void bheap_destroy(struct bheap *bh) {
  if (bh == NULL)
    return;
  if (bh->heap != NULL)
    free(bh->heap);
  free(bh);
}

int _bheap_left(int i) {
  return 2*i + 1;
}

int _bheap_right(int i) {
  return 2*i + 2;
}

static inline int _bheap_parent(int i) {
  return (i-1) / 2;
}

static inline void _bheap_swap(struct bheap *bh, int i, int j) {
  /* TODO: could we potentially use signed integer swap trick on pointers? */
  const char *tmp = bh->heap[i];
  bh->heap[i] = bh->heap[j];
  bh->heap[j] = tmp;
}

static inline void _bheap_bubble_up(struct bheap *bh, int i) {
  int p = _bheap_parent(i);
  while (i > 0 && strcmp(bh->heap[i], bh->heap[p]) < 0) {
    _bheap_swap(bh, i, p);
    i = p;
    p = _bheap_parent(i);
  }
}

static inline void _heap_bubble_down(struct bheap *bh, int i) {
}

/* bheap_add: Add element to binary heap bh. Does not copy the string.
 *             Clients will need to free them before destroying the heap.
 * returns: 0  - element was added
 *          1  - element was present (and not added)
 *          -1 - heap size overflowed
 */
int bheap_add(struct bheap *bh, const char *element) {
  if (bh->len == bh->max)
    return -1;
  bh->heap[bh->len] = element;
  _bheap_bubble_up(bh, bh->len);
  bh->len++;
  return 0;
}
