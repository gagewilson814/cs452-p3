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
    return 0;
  }

  size_t kVal = 0;
  // Start with the smallest power-of-two value (2^0 = 1).
  for (size_t value = 1; value < bytes; value <<= 1) {
    kVal++;
  }
  return kVal;
}

struct avail *buddy_calc(struct buddy_pool *pool, struct avail *buddy) {
  // Add validation
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

  // Check if the buddy block is within the bounds of the pool
  if (buddyBlock < base || buddyBlock >= base + pool->numbytes) {
    return NULL;
  }
  // Check if the buddy block is available
  if (buddyBlockPtr->tag != BLOCK_AVAIL) {
    return NULL;
  }

  return buddyBlockPtr;
}

static struct avail *recursive_split(struct buddy_pool *pool, struct avail *block,
                                       size_t current_k, size_t required_k) {
  if (current_k == required_k) {
    // Base case: block is exactly the size we need.
    block->tag = BLOCK_RESERVED;
    block->kval = required_k;
    return block;
  }

  // Otherwise, split the block.
  current_k--; // Decrease the size by one exponent.
  size_t block_size = UINT64_C(1) << current_k;

  // Compute the address of the buddy block.
  struct avail *buddy = (struct avail *)((char *)block + block_size);
  buddy->kval = current_k;
  buddy->tag = BLOCK_AVAIL;

  // Insert the buddy block into the free list for the current_k level.
  buddy->next = pool->avail[current_k].next;
  buddy->prev = &pool->avail[current_k];
  pool->avail[current_k].next->prev = buddy;
  pool->avail[current_k].next = buddy;

  // Recursively split the original block.
  return recursive_split(pool, block, current_k, required_k);
}

void *buddy_malloc(struct buddy_pool *pool, size_t size) {
  // Validate input parameters.
  if (pool == NULL || size == 0) {
    errno = EINVAL;
    return NULL;
  }

  // Calculate the total size needed (user data + metadata header).
  size_t total_size = size + sizeof(struct avail);

  // Determine the minimum block size (k value) required.
  size_t required_k = btok(total_size);
  if (required_k < SMALLEST_K) {
    required_k = SMALLEST_K;
  }

  // Find the smallest free block that can accommodate the required size.
  size_t current_k = required_k;
  while (current_k <= pool->kval_m &&
         pool->avail[current_k].next == &pool->avail[current_k]) {
    current_k++;
  }

  // If no block is available, set error and return NULL.
  if (current_k > pool->kval_m) {
    errno = ENOMEM;
    return NULL;
  }

  // Remove the block from the free list.
  struct avail *block = pool->avail[current_k].next;
  block->prev->next = block->next;
  block->next->prev = block->prev;

  // Recursively split the block until it matches the required size.
  block = recursive_split(pool, block, current_k, required_k);

  // Return pointer to user-accessible memory (skip the metadata header).
  return (void *)(block + 1);
}



void buddy_free(struct buddy_pool *pool, void *ptr) {
  if (pool == NULL || ptr == NULL) {
    return;
  }

  // Retrieve the block header.
  struct avail *block = ((struct avail *)ptr) - 1;
  block->tag = BLOCK_AVAIL;

  // Attempt to merge with buddy as long as possible.
  while (block->kval < pool->kval_m) {
    struct avail *buddy = buddy_calc(pool, block);
    if (buddy == NULL || buddy->kval != block->kval) {
      break;
    }
    // Remove buddy from its free list.
    buddy->prev->next = buddy->next;
    buddy->next->prev = buddy->prev;
    if ((uintptr_t)buddy < (uintptr_t)block) {
      block = buddy;
    }
    block->kval++;
  }

  // Add the block to the appropriate free list
  size_t k = block->kval;
  block->next = pool->avail[k].next;
  block->prev = &pool->avail[k];
  pool->avail[k].next->prev = block;
  pool->avail[k].next = block;
}

#define UNUSED(x) (void)x
void *buddy_realloc(struct buddy_pool *pool, void *ptr, size_t size) {
  UNUSED(pool);
  UNUSED(ptr);
  UNUSED(size);
  errno = ENOSYS; // Not implemented.
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

  // Set all blocks to empty. We are using circular lists so the first elements
  // just point to an available block. Thus the tag, and kval feild are unused
  // burning a small bit of memory but making the code more readable. We mark
  // these blocks as UNUSED to aid in debugging.
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
