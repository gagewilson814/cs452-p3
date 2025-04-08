#include <assert.h>
#include <execinfo.h>
#include <signal.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <time.h>
#include <unistd.h>
#ifdef __APPLE__
#include <sys/errno.h>
#else
#include <errno.h>
#endif

#include "lab.h"

#define handle_error_and_die(msg)                                              \
  do {                                                                         \
    perror(msg);                                                               \
    raise(SIGKILL);                                                            \
  } while (0)

/**
 * @brief Convert bytes to the correct K value
 *
 * @param bytes the number of bytes
 * @return size_t the K value that will fit bytes
 */
size_t btok(size_t bytes) {

  // If no bytes are requested, return 0 immediately.
  if (bytes == 0) {
    return UINT64_C(0);
  }

  size_t kVal = UINT64_C(0);
  // Start with the smallest power-of-two value and increase to fit
  for (size_t value = UINT64_C(1); value < bytes; value <<= UINT64_C(1)) {
    kVal++;
  }
  return kVal;
}

struct avail *buddy_calc(struct buddy_pool *pool, struct avail *buddy) {
  if (pool == NULL || buddy == NULL) {
    return NULL;
  }

  uintptr_t base = (uintptr_t)pool->base;
  uintptr_t block = (uintptr_t)buddy;
  uintptr_t offset = block - base;                  // calculate offset
  uintptr_t blockSize = UINT64_C(1) << buddy->kval; // calculate block size
  uintptr_t buddyOffset = offset ^ blockSize;       // calculate buddy offset
  uintptr_t buddyBlock = base + buddyOffset;
  struct avail *buddyBlockPtr = (struct avail *)buddyBlock; // cast to avail

  return buddyBlockPtr;
}

void *buddy_malloc(struct buddy_pool *pool, size_t size) {

  // get the kval for the requested size with enough room for the tag and kval
  // fields
  size_t k = btok(size + sizeof(struct avail));

  // R1 Find a block
  struct avail *L = NULL;
  size_t j = 0;
  for (j = k; j <= pool->kval_m; j++) {
    if (pool->avail[j].next != &pool->avail[j]) {
      L = pool->avail[j].next;
      break;
    }
  }

  // There was not enough memory to satisfy the request thus we need to set
  // error and return NULL
  if (L == NULL) {
    errno = ENOMEM;
    return NULL;
  }

  // R2 Remove from list
  L->prev->next = L->next;
  L->next->prev = L->prev;
  L->tag = BLOCK_RESERVED;

  char *P = (char *)L;

  // R3 Split required?
  while (j > k) {
    j--;

    size_t new_size = UINT64_C(1) << j;

    struct avail *buddy = (struct avail *)(P + new_size);
    buddy->tag = BLOCK_AVAIL;
    buddy->kval = j;

    buddy->next = pool->avail[j].next;
    buddy->prev = &pool->avail[j];
    pool->avail[j].next->prev = buddy;
    pool->avail[j].next = buddy;

    ((struct avail *)P)->kval = j;
  }

  return (void *)P + sizeof(struct avail);
}

void buddy_free(struct buddy_pool *pool, void *ptr) {
  if (ptr == NULL || pool == NULL) {
    return;
  }

  struct avail *L = (struct avail *)ptr - UINT64_C(1);

  L->tag = BLOCK_AVAIL;
  size_t k = L->kval;

  while (k < pool->kval_m) {
    struct avail *P = buddy_calc(pool, L);
    if (P == NULL || P->tag == BLOCK_RESERVED ||
        (P->tag == BLOCK_AVAIL && P->kval != k)) {
      break;
    }

    P->prev->next = P->next;
    P->next->prev = P->prev;

    if ((uintptr_t)P < (uintptr_t)L)
      L = P;

    k++;
    L->kval = k;
  }

  L->next = pool->avail[k].next;
  L->prev = &pool->avail[k];
  pool->avail[k].next->prev = L;
  pool->avail[k].next = L;
}

// Didn't implement as it's optional for undergrad
#define UNUSED(x) (void)x
void *buddy_realloc(struct buddy_pool *pool, void *ptr, size_t size) {
  UNUSED(pool);
  UNUSED(ptr);
  UNUSED(size);
  errno = ENOSYS;
  return NULL;
}

void buddy_init(struct buddy_pool *pool, size_t size) {
  size_t kval = 0;
  if (size == 0)
    kval = DEFAULT_K;
  else
    kval = btok(size);

  if (kval < MIN_K)
    kval = MIN_K;
  if (kval > MAX_K)
    kval = MAX_K - 1;

  // make sure pool struct is cleared out
  memset(pool, 0, sizeof(struct buddy_pool));
  pool->kval_m = kval;
  pool->numbytes = (UINT64_C(1) << pool->kval_m);
  // Memory map a block of raw memory to manage
  pool->base = mmap(NULL,                        /*addr to map to*/
                    pool->numbytes,              /*length*/
                    PROT_READ | PROT_WRITE,      /*prot*/
                    MAP_PRIVATE | MAP_ANONYMOUS, /*flags*/
                    -1, /*fd -1 when using MAP_ANONYMOUS*/
                    0   /* offset 0 when using MAP_ANONYMOUS*/
  );
  if (MAP_FAILED == pool->base) {
    handle_error_and_die("buddy_init avail array mmap failed");
  }

  // Set all blocks to empty. We are using circular lists so the first
  // elements just point to an available block. Thus the tag, and kval feild
  // are unused burning a small bit of memory but making the code more
  // readable. We mark these blocks as UNUSED to aid in debugging.
  for (size_t i = 0; i <= kval; i++) {
    pool->avail[i].next = pool->avail[i].prev = &pool->avail[i];
    pool->avail[i].kval = i;
    pool->avail[i].tag = BLOCK_UNUSED;
  }

  // Add in the first block
  pool->avail[kval].next = pool->avail[kval].prev = (struct avail *)pool->base;
  struct avail *m = pool->avail[kval].next;
  m->tag = BLOCK_AVAIL;
  m->kval = kval;
  m->next = m->prev = &pool->avail[kval];
}

void buddy_destroy(struct buddy_pool *pool) {
  int rval = munmap(pool->base, pool->numbytes);
  if (-1 == rval) {
    handle_error_and_die("buddy_destroy avail array");
  }
  // Zero out the array so it can be reused it needed
  memset(pool, 0, sizeof(struct buddy_pool));
}

#define UNUSED(x) (void)x

/**
 * This function can be useful to visualize the bits in a block. This can
 * help when figuring out the buddy_calc function!
 */
// static void printb(unsigned long int b) {
//   size_t bits = sizeof(b) * 8;
//   unsigned long int curr = UINT64_C(1) << (bits - 1);
//   for (size_t i = 0; i < bits; i++) {
//     if (b & curr) {
//       printf("1");
//     } else {
//       printf("0");
//     }
//     curr >>= 1L;
//   }
// }
