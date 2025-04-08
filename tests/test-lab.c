#include <assert.h>
#include <stdlib.h>
#include <time.h>
#ifdef __APPLE__
#include <sys/errno.h>
#else
#include <errno.h>
#endif
#include "../src/lab.h"
#include "harness/unity.h"

void setUp(void) {
  // set stuff up here
}

void tearDown(void) {
  // clean stuff up here
}

/**
 * Check the pool to ensure it is full.
 */
void check_buddy_pool_full(struct buddy_pool *pool) {
  // A full pool should have all values 0-(kval-1) as empty
  for (size_t i = 0; i < pool->kval_m; i++) {
    assert(pool->avail[i].next == &pool->avail[i]);
    assert(pool->avail[i].prev == &pool->avail[i]);
    assert(pool->avail[i].tag == BLOCK_UNUSED);
    assert(pool->avail[i].kval == i);
  }

  // The avail array at kval should have the base block
  assert(pool->avail[pool->kval_m].next->tag == BLOCK_AVAIL);
  assert(pool->avail[pool->kval_m].next->next == &pool->avail[pool->kval_m]);
  assert(pool->avail[pool->kval_m].prev->prev == &pool->avail[pool->kval_m]);

  // Check to make sure the base address points to the starting pool.
  assert(pool->avail[pool->kval_m].next == pool->base);
}

/**
 * Check the pool to ensure it is empty.
 */
void check_buddy_pool_empty(struct buddy_pool *pool) {
  // An empty pool should have all values 0-(kval) as empty
  for (size_t i = 0; i <= pool->kval_m; i++) {
    assert(pool->avail[i].next == &pool->avail[i]);
    assert(pool->avail[i].prev == &pool->avail[i]);
    assert(pool->avail[i].tag == BLOCK_UNUSED);
    assert(pool->avail[i].kval == i);
  }
}

/**
 * Test allocating 1 byte to make sure we split the blocks all the way down
 * to MIN_K size. Then free the block and ensure we end up with a full
 * memory pool again.
 */
void test_buddy_malloc_one_byte(void) {
  fprintf(stderr, "-> Test allocating and freeing 1 byte\n");
  struct buddy_pool pool;
  int kval = MIN_K;
  size_t size = UINT64_C(1) << kval;
  buddy_init(&pool, size);
  void *mem = buddy_malloc(&pool, 1);
  // Make sure correct kval was allocated
  buddy_free(&pool, mem);
  check_buddy_pool_full(&pool);
  buddy_destroy(&pool);
}

/**
 * Test the allocation of one massive block that should consume the entire
 * memory pool and makes sure that after the pool is empty we correctly fail
 * subsequent calls.
 */
void test_buddy_malloc_one_large(void) {
  fprintf(stderr, "-> Testing size that will consume entire memory pool\n");
  struct buddy_pool pool;
  size_t bytes = UINT64_C(1) << MIN_K;
  buddy_init(&pool, bytes);

  // Ask for an exact K value to be allocated. This test makes assumptions on
  // the internal details of buddy_init.
  size_t ask = bytes - sizeof(struct avail);
  void *mem = buddy_malloc(&pool, ask);
  assert(mem != NULL);

  // Move the pointer back and make sure we got what we expected.
  struct avail *tmp = (struct avail *)mem - 1;
  assert(tmp->kval == MIN_K);
  assert(tmp->tag == BLOCK_RESERVED);
  check_buddy_pool_empty(&pool);

  // Verify that a call on an empty pool fails as expected and errno is set to
  // ENOMEM.
  void *fail = buddy_malloc(&pool, 5);
  assert(fail == NULL);
  assert(errno == ENOMEM);

  // Free the memory and then check to make sure everything is OK.
  buddy_free(&pool, mem);
  check_buddy_pool_full(&pool);
  buddy_destroy(&pool);
}

/**
 * Test to make sure that the struct buddy_pool is correct and all fields
 * have been properly set (kval_m, avail[kval_m], and base pointer) after a call
 * to init.
 */
void test_buddy_init(void) {
  fprintf(stderr, "-> Testing buddy init\n");
  // Loop through all kval MIN_K-DEFAULT_K and make sure we get the correct
  // amount allocated. We will check all the pointer offsets to ensure the pool
  // is all configured correctly.
  for (size_t i = MIN_K; i <= DEFAULT_K; i++) {
    size_t size = UINT64_C(1) << i;
    struct buddy_pool pool;
    buddy_init(&pool, size);
    check_buddy_pool_full(&pool);
    buddy_destroy(&pool);
  }
}

/**
 * Test 4: Perform a random allocation/free sequence.
 * Allocate a series of blocks of random sizes, free them in random order,
 * and verify that the buddy pool is completely coalesced afterward.
 */
void test_buddy_random_alloc_free(void) {
  fprintf(stderr, "-> Testing random allocation and free sequence\n");
  struct buddy_pool pool;
  size_t pool_size = UINT64_C(1) << DEFAULT_K;
  buddy_init(&pool, pool_size);

  const int num_allocs = 10;
  void *blocks[num_allocs];

  // Allocate random-sized blocks.
  for (int i = 0; i < num_allocs; i++) {
    size_t size = rand() % 128 + 1; // Sizes between 1 and 128 bytes.
    blocks[i] = buddy_malloc(&pool, size);
    TEST_ASSERT_NOT_NULL(blocks[i]);
  }

  // Free the blocks in random order.
  for (int i = 0; i < num_allocs; i++) {
    int idx = rand() % num_allocs;
    if (blocks[idx] != NULL) {
      buddy_free(&pool, blocks[idx]);
      blocks[idx] = NULL;
    }
  }

  // Free any remaining blocks.
  for (int i = 0; i < num_allocs; i++) {
    if (blocks[i] != NULL) {
      buddy_free(&pool, blocks[i]);
      blocks[i] = NULL;
    }
  }

  // Check that the pool is fully coalesced.
  check_buddy_pool_full(&pool);
  buddy_destroy(&pool);
}

/**
 * Test buddy coalescing by allocating two minimal blocks from a pool
 * that's exactly twice the minimal size, freeing them, and verifying that
 * the pool is fully merged (coalesced).
 */
void test_buddy_coalescing(void) {
  fprintf(stderr, "-> Testing buddy coalescing of two minimal blocks\n");
  struct buddy_pool pool;
  // Create a pool of size 2 minimal blocks.
  size_t pool_size = UINT64_C(1) << (MIN_K + 1);
  buddy_init(&pool, pool_size);

  void *block1 = buddy_malloc(&pool, 1);
  void *block2 = buddy_malloc(&pool, 1);
  TEST_ASSERT_NOT_NULL(block1);
  TEST_ASSERT_NOT_NULL(block2);

  // Free both blocks to trigger coalescing.
  buddy_free(&pool, block1);
  buddy_free(&pool, block2);

  // Now the pool should be fully coalesced.
  check_buddy_pool_full(&pool);
  buddy_destroy(&pool);
}

/**
 * Test buddy allocation with a request for 0 bytes.
 * (Depending on the design of buddy_malloc, this may either return a minimal
 * block or be treated as an error. Here, we assume a minimal block is
 * returned.)
 */
void test_buddy_malloc_zero(void) {
  fprintf(stderr, "-> Testing buddy allocation with 0 bytes\n");
  struct buddy_pool pool;
  size_t pool_size = UINT64_C(1) << DEFAULT_K;
  buddy_init(&pool, pool_size);

  void *block = buddy_malloc(&pool, 0);
  TEST_ASSERT_NOT_NULL(block);

  buddy_free(&pool, block);
  check_buddy_pool_full(&pool);
  buddy_destroy(&pool);
}

/**
 * Test reallocation: Allocate a block, free it, and then allocate again.
 * This ensures that freed memory is properly recycled.
 */
void test_buddy_alloc_realloc(void) {
  fprintf(stderr, "-> Testing buddy allocation, free, and reallocation\n");
  struct buddy_pool pool;
  size_t pool_size = UINT64_C(1) << DEFAULT_K;
  buddy_init(&pool, pool_size);

  void *block1 = buddy_malloc(&pool, 10);
  TEST_ASSERT_NOT_NULL(block1);

  buddy_free(&pool, block1);

  // Reallocate a block of the same size.
  void *block2 = buddy_malloc(&pool, 10);
  TEST_ASSERT_NOT_NULL(block2);

  buddy_free(&pool, block2);
  check_buddy_pool_full(&pool);
  buddy_destroy(&pool);
}

/**
 * Test allocation failure when requesting oversized memory.
 * Request more than the available pool size and ensure that the allocation
 * fails.
 */
void test_buddy_alloc_failure(void) {
  fprintf(stderr, "-> Testing allocation failure for oversized requests\n");
  struct buddy_pool pool;
  size_t pool_size = UINT64_C(1) << DEFAULT_K;
  buddy_init(&pool, pool_size);

  // Request more than the available memory.
  void *block = buddy_malloc(&pool, pool_size + 1);
  TEST_ASSERT_NULL(block);

  // The pool should remain unchanged.
  check_buddy_pool_full(&pool);
  buddy_destroy(&pool);
}

/**
 * Stress test the buddy allocator by allocating 1-byte blocks repeatedly until
 * the pool is exhausted, then freeing all and verifying the pool is fully
 * coalesced.
 */
void test_buddy_stress_alloc_free(void) {
  fprintf(stderr, "-> Testing buddy allocator stress test\n");
  struct buddy_pool pool;
  size_t pool_size = UINT64_C(1) << DEFAULT_K;
  buddy_init(&pool, pool_size);

#define MAX_ALLOCS 1024
  void *blocks[MAX_ALLOCS];
  int count = 0;

  // Allocate 1-byte blocks until no more memory can be allocated.
  while (count < MAX_ALLOCS) {
    void *block = buddy_malloc(&pool, 1);
    if (block == NULL)
      break;
    blocks[count++] = block;
  }

  // Free all allocated blocks.
  for (int i = 0; i < count; i++) {
    buddy_free(&pool, blocks[i]);
  }

  check_buddy_pool_full(&pool);
  buddy_destroy(&pool);
}

int main(void) {
  time_t t;
  unsigned seed = (unsigned)time(&t);
  fprintf(stderr, "Random seed: %d\n", seed);
  srand(seed);
  printf("Running memory tests.\n");

  UNITY_BEGIN();
  RUN_TEST(test_buddy_init);
  RUN_TEST(test_buddy_malloc_one_byte);
  RUN_TEST(test_buddy_malloc_one_large);
  RUN_TEST(test_buddy_random_alloc_free);
  RUN_TEST(test_buddy_coalescing);
  RUN_TEST(test_buddy_malloc_zero);
  RUN_TEST(test_buddy_alloc_realloc);
  RUN_TEST(test_buddy_alloc_failure);
  RUN_TEST(test_buddy_stress_alloc_free);
  return UNITY_END();
}
