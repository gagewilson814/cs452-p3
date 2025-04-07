#include <assert.h>
#include <stdlib.h>
#include <time.h>
#include <stdint.h>
#ifdef __APPLE__
#include <sys/errno.h>
#else
#include <errno.h>
#endif
#include "../src/lab.h"
#include "harness/unity.h"

void setUp(void) {
  // set stuff up here if needed
}

void tearDown(void) {
  // clean stuff up here if needed
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

  // Check to make sure the base address points to the starting pool
  // If this fails either buddy_init is wrong or we have corrupted the
  // buddy_pool struct.
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
 * Existing Tests
 */

void test_buddy_malloc_one_byte(void) {
  fprintf(stderr, "->Test allocating and freeing 1 byte\n");
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

void test_buddy_malloc_one_large(void) {
  fprintf(stderr, "->Testing size that will consume entire memory pool\n");
  struct buddy_pool pool;
  size_t bytes = UINT64_C(1) << MIN_K;
  buddy_init(&pool, bytes);

  // Ask for an exact K value to be allocated. This test makes assumptions on
  // the internal details of buddy_init.
  size_t ask = bytes - sizeof(struct avail);
  void *mem = buddy_malloc(&pool, ask);
  assert(mem != NULL);

  // Move the pointer back and make sure we got what we expected
  struct avail *tmp = (struct avail *)mem - 1;
  assert(tmp->kval == MIN_K);
  assert(tmp->tag == BLOCK_RESERVED);
  check_buddy_pool_empty(&pool);

  // Verify that a call on an empty pool fails as expected and errno is set to
  // ENOMEM.
  void *fail = buddy_malloc(&pool, 5);
  assert(fail == NULL);
  assert(errno == ENOMEM);

  // Free the memory and then check to make sure everything is OK
  buddy_free(&pool, mem);
  check_buddy_pool_full(&pool);
  buddy_destroy(&pool);
}

void test_buddy_init(void) {
  fprintf(stderr, "->Testing buddy init\n");
  // Loop through all kval MIN_K-DEFAULT_K and make sure we get the correct
  // amount allocated. We will check all the pointer offsets to ensure the pool
  // is all configured correctly
  for (size_t i = MIN_K; i <= DEFAULT_K; i++) {
    size_t size = UINT64_C(1) << i;
    struct buddy_pool pool;
    buddy_init(&pool, size);
    check_buddy_pool_full(&pool);
    buddy_destroy(&pool);
  }
}

/**
 * Additional Tests
 */

/* Test that invalid parameters (null pool or 0 size) result in an error. */
void test_buddy_malloc_invalid(void) {
  fprintf(stderr, "-> Testing buddy_malloc with invalid parameters\n");
  struct buddy_pool pool;
  buddy_init(&pool, 1 << MIN_K);
  
  errno = 0;
  void *mem = buddy_malloc(&pool, 0);
  TEST_ASSERT_NULL(mem);
  TEST_ASSERT_EQUAL(EINVAL, errno);

  errno = 0;
  mem = buddy_malloc(NULL, 10);
  TEST_ASSERT_NULL(mem);
  TEST_ASSERT_EQUAL(EINVAL, errno);

  buddy_destroy(&pool);
}

/* Test that a request larger than the pool's capacity fails appropriately. */
void test_buddy_malloc_too_big(void) {
  fprintf(stderr, "-> Testing buddy_malloc with request too large for pool\n");
  struct buddy_pool pool;
  size_t pool_size = 1 << MIN_K;
  buddy_init(&pool, pool_size);
  
  errno = 0;
  // Request a size that is definitely too big considering metadata overhead.
  void *mem = buddy_malloc(&pool, pool_size);
  TEST_ASSERT_NULL(mem);
  TEST_ASSERT_EQUAL(ENOMEM, errno);
  
  buddy_destroy(&pool);
}

/* Test that freeing two buddy blocks results in correct coalescing. */
void test_buddy_coalescing(void) {
  fprintf(stderr, "-> Testing buddy coalescing after freeing buddy blocks\n");
  struct buddy_pool pool;
  size_t pool_size = 1 << (MIN_K + 1); // A pool that can be split into two minimum blocks
  buddy_init(&pool, pool_size);
  
  void *mem1 = buddy_malloc(&pool, 1);
  TEST_ASSERT_NOT_NULL(mem1);
  void *mem2 = buddy_malloc(&pool, 1);
  TEST_ASSERT_NOT_NULL(mem2);
  
  // Free in different order to check that coalescing works correctly.
  buddy_free(&pool, mem2);
  buddy_free(&pool, mem1);
  
  check_buddy_pool_full(&pool);
  buddy_destroy(&pool);
}

/* Test repeated allocation and free cycles to ensure the pool resets correctly. */
void test_buddy_repeat_alloc_free(void) {
  fprintf(stderr, "-> Testing repeated allocation and free\n");
  struct buddy_pool pool;
  size_t pool_size = 1 << DEFAULT_K;
  buddy_init(&pool, pool_size);
  
  for (int i = 0; i < 50; i++) {
    void *mem = buddy_malloc(&pool, 1);
    TEST_ASSERT_NOT_NULL(mem);
    buddy_free(&pool, mem);
    check_buddy_pool_full(&pool);
  }
  
  buddy_destroy(&pool);
}

/* Test random allocations until exhaustion and then free them in random order. */
void test_buddy_random_allocations(void) {
  fprintf(stderr, "-> Testing random allocations until exhaustion then free\n");
  struct buddy_pool pool;
  size_t pool_size = 1 << DEFAULT_K;
  buddy_init(&pool, pool_size);
  
  #define MAX_ALLOCS 100
  void *allocs[MAX_ALLOCS];
  int count = 0;
  
  // Allocate blocks of random sizes until buddy_malloc fails or maximum reached.
  while (count < MAX_ALLOCS) {
    size_t req = (rand() % (1 << MIN_K)) + 1; // Random request between 1 and 2^(MIN_K)
    void *mem = buddy_malloc(&pool, req);
    if (!mem)
      break;
    allocs[count++] = mem;
  }
  
  // Shuffle the allocated pointers to free them in random order.
  for (int i = count - 1; i > 0; i--) {
    int j = rand() % (i + 1);
    void *temp = allocs[i];
    allocs[i] = allocs[j];
    allocs[j] = temp;
  }
  
  for (int i = 0; i < count; i++) {
    buddy_free(&pool, allocs[i]);
  }
  
  check_buddy_pool_full(&pool);
  buddy_destroy(&pool);
}

/* Test the btok function for correct exponent calculation. */
void test_btok(void) {
  fprintf(stderr, "-> Testing btok function\n");
  TEST_ASSERT_EQUAL_UINT64(0, btok(0));  // 0 bytes -> 0
  TEST_ASSERT_EQUAL_UINT64(0, btok(1));  // 1 is 2^0
  TEST_ASSERT_EQUAL_UINT64(1, btok(2));  // 2 is 2^1
  TEST_ASSERT_EQUAL_UINT64(2, btok(3));  // smallest power-of-2 >=3 is 4 (2^2)
  TEST_ASSERT_EQUAL_UINT64(2, btok(4));  // 4 is 2^2
  TEST_ASSERT_EQUAL_UINT64(3, btok(5));  // smallest power-of-2 >=5 is 8 (2^3)
  TEST_ASSERT_EQUAL_UINT64(3, btok(6));  // smallest power-of-2 >=6 is 8 (2^3)
  TEST_ASSERT_EQUAL_UINT64(3, btok(7));  // smallest power-of-2 >=7 is 8 (2^3)
  TEST_ASSERT_EQUAL_UINT64(3, btok(8));  // smallest power-of-2 >=8 is 8 (2^3)
  TEST_ASSERT_EQUAL_UINT64(4, btok(9));  // smallest power-of-2 >=9 is 16 (2^4)
  TEST_ASSERT_EQUAL_UINT64(4, btok(10)); // smallest power-of-2 >=10 is 16 (2^4)
  TEST_ASSERT_EQUAL_UINT64(4, btok(11)); // smallest power-of-2 >=11 is 16 (2^4)
  TEST_ASSERT_EQUAL_UINT64(4, btok(12)); // smallest power-of-2 >=12 is 16 (2^4)
}

/* Test that allocations are properly aligned */
void test_buddy_alignment(void) {
  fprintf(stderr, "-> Testing alignment of buddy_malloc allocations\n");
  struct buddy_pool pool;
  size_t pool_size = 1 << DEFAULT_K;
  buddy_init(&pool, pool_size);
  
  void *mem = buddy_malloc(&pool, 5);
  TEST_ASSERT_NOT_NULL(mem);
  // Check that the pointer is aligned to at least the size of a pointer.
  TEST_ASSERT_EQUAL_UINT64(0, ((uintptr_t)mem) % sizeof(void*));
  
  buddy_free(&pool, mem);
  buddy_destroy(&pool);
}

/* Test that allocated blocks do not overlap in memory. */
void test_buddy_non_overlapping(void) {
  fprintf(stderr, "-> Testing non-overlapping allocations\n");
  struct buddy_pool pool;
  size_t pool_size = 1 << (MIN_K + 3); // larger pool for multiple allocations
  buddy_init(&pool, pool_size);
  
  #define NUM_BLOCKS 10
  void *blocks[NUM_BLOCKS];
  for (int i = 0; i < NUM_BLOCKS; i++) {
    blocks[i] = buddy_malloc(&pool, 1);
    TEST_ASSERT_NOT_NULL(blocks[i]);
  }
  // Check that none of the allocated blocks overlap.
  for (int i = 0; i < NUM_BLOCKS; i++) {
    struct avail *hdr_i = ((struct avail *)blocks[i]) - 1;
    size_t block_size_i = (size_t)1 << hdr_i->kval;
    uintptr_t start_i = (uintptr_t)hdr_i;
    uintptr_t end_i = start_i + block_size_i;
    for (int j = i + 1; j < NUM_BLOCKS; j++) {
      struct avail *hdr_j = ((struct avail *)blocks[j]) - 1;
      size_t block_size_j = (size_t)1 << hdr_j->kval;
      uintptr_t start_j = (uintptr_t)hdr_j;
      uintptr_t end_j = start_j + block_size_j;
      // The blocks must not overlap.
      TEST_ASSERT_TRUE(end_i <= start_j || end_j <= start_i);
    }
  }
  for (int i = 0; i < NUM_BLOCKS; i++) {
    buddy_free(&pool, blocks[i]);
  }
  check_buddy_pool_full(&pool);
  buddy_destroy(&pool);
}

/* Test allocations with sizes exactly at the boundaries between block sizes. */
void test_buddy_malloc_boundaries(void) {
  fprintf(stderr, "-> Testing buddy_malloc at block boundaries\n");
  size_t pool_size = 1 << DEFAULT_K;
  struct buddy_pool pool;
  buddy_init(&pool, pool_size);
  
  // For each possible block size between MIN_K and DEFAULT_K, request a block
  // with size exactly (2^n - sizeof(struct avail)).
  for (size_t n = MIN_K; n <= DEFAULT_K; n++) {
    size_t req_size = ((size_t)1 << n) - sizeof(struct avail);
    void *mem = buddy_malloc(&pool, req_size);
    TEST_ASSERT_NOT_NULL(mem);
    struct avail *hdr = ((struct avail *)mem) - 1;
    // Verify that the allocated block is exactly of size 2^n.
    TEST_ASSERT_EQUAL_UINT64(n, hdr->kval);
    buddy_free(&pool, mem);
  }
  check_buddy_pool_full(&pool);
  buddy_destroy(&pool);
}

void test_buddy_free_null(void) {
  fprintf(stderr, "-> Testing buddy_free with a NULL pointer\n");
  struct buddy_pool pool;
  size_t pool_size = 1 << MIN_K;
  buddy_init(&pool, pool_size);
  buddy_free(&pool, NULL);
  check_buddy_pool_full(&pool);
  buddy_destroy(&pool);
}

int main(void) {
  time_t t;
  unsigned seed = (unsigned)time(&t);
  fprintf(stderr, "Random seed:%d\n", seed);
  srand(seed);
  printf("Running memory tests.\n");

  UNITY_BEGIN();
  RUN_TEST(test_buddy_init);
  RUN_TEST(test_buddy_malloc_one_byte);
  RUN_TEST(test_buddy_malloc_one_large);
  RUN_TEST(test_buddy_malloc_invalid);
  RUN_TEST(test_buddy_malloc_too_big);
  RUN_TEST(test_buddy_coalescing);
  RUN_TEST(test_buddy_repeat_alloc_free);
  RUN_TEST(test_buddy_random_allocations);
  RUN_TEST(test_btok);
  RUN_TEST(test_buddy_alignment);
  RUN_TEST(test_buddy_non_overlapping);
  RUN_TEST(test_buddy_malloc_boundaries);
  RUN_TEST(test_buddy_free_null);
  return UNITY_END();
}
