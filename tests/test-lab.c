#include <assert.h>
#include <stdint.h>
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

// helper function to check the integrity of the free list
void check_free_list_integrity(struct buddy_pool *pool) {
  for (size_t i = 0; i <= pool->kval_m; i++) {
    struct avail *sentinel = &pool->avail[i];
    struct avail *node = sentinel->next;
    while (node != sentinel) {
      // Each node’s next->prev should point back to the node.
      TEST_ASSERT_EQUAL_PTR(node, node->next->prev);
      node = node->next;
    }
  }
}

void test_pool_initial_state(void) {
  struct buddy_pool pool;
  size_t poolSize = 1 << MIN_K; // pool size passed to buddy_init
  buddy_init(&pool, poolSize);

  // All free-list sentinels at levels 0 .. pool->kval_m-1 should be empty.
  for (size_t i = 0; i < pool.kval_m; i++) {
    TEST_ASSERT_EQUAL_PTR(&pool.avail[i], pool.avail[i].next);
    TEST_ASSERT_EQUAL_PTR(&pool.avail[i], pool.avail[i].prev);
    TEST_ASSERT_EQUAL_UINT(BLOCK_UNUSED, pool.avail[i].tag);
    TEST_ASSERT_EQUAL_UINT(i, pool.avail[i].kval);
  }
  // The highest level should have a free block that holds the entire pool.
  // Replace TEST_ASSERT_NOT_EQUAL_PTR with an equivalent check:
  TEST_ASSERT_TRUE(pool.avail[pool.kval_m].next != &pool.avail[pool.kval_m]);
  TEST_ASSERT_EQUAL_PTR(pool.base, pool.avail[pool.kval_m].next);

  check_free_list_integrity(&pool);
  buddy_destroy(&pool);
}

/* Test that after several allocations the free blocks all lie within the pool's
 * address range */
void test_pool_after_allocations(void) {
  struct buddy_pool pool;
  buddy_init(&pool, 1 << (MIN_K + 1));

  void *a = buddy_malloc(&pool, 100);
  void *b = buddy_malloc(&pool, 150);
  void *c = buddy_malloc(&pool, 200);

  check_free_list_integrity(&pool);

  // For each level, verify that all free blocks lie within the mapped pool.
  for (size_t i = 0; i <= pool.kval_m; i++) {
    struct avail *sentinel = &pool.avail[i];
    struct avail *node = sentinel->next;
    while (node != sentinel) {
      uintptr_t addr = (uintptr_t)node;
      TEST_ASSERT_TRUE(addr >= (uintptr_t)pool.base);
      TEST_ASSERT_TRUE(addr < (uintptr_t)pool.base + pool.numbytes);
      node = node->next;
    }
  }

  buddy_free(&pool, b);
  check_free_list_integrity(&pool);
  buddy_free(&pool, a);
  buddy_free(&pool, c);

  // After all frees, the entire pool should be merged.
  for (size_t i = 0; i < pool.kval_m; i++) {
    TEST_ASSERT_EQUAL_PTR(&pool.avail[i], pool.avail[i].next);
    TEST_ASSERT_EQUAL_PTR(&pool.avail[i], pool.avail[i].prev);
    TEST_ASSERT_EQUAL_UINT(BLOCK_UNUSED, pool.avail[i].tag);
  }
  TEST_ASSERT_EQUAL_PTR(pool.base, pool.avail[pool.kval_m].next);
  check_free_list_integrity(&pool);
  buddy_destroy(&pool);
}

/* Test that the total free memory equals the pool size when no allocation is
 * outstanding */
void test_total_free_memory_calculation(void) {
  struct buddy_pool pool;
  buddy_init(&pool, 1 << (MIN_K + 1));

  size_t total_free = 0;
  for (size_t i = 0; i <= pool.kval_m; i++) {
    struct avail *sentinel = &pool.avail[i];
    struct avail *node = sentinel->next;
    while (node != sentinel) {
      total_free += (UINT64_C(1) << node->kval);
      node = node->next;
    }
  }
  TEST_ASSERT_EQUAL_UINT(pool.numbytes, total_free);
  buddy_destroy(&pool);
}

void test_btok_zero(void) {
  size_t header = sizeof(struct avail);
  int expected = SMALLEST_K;
  while ((UINT64_C(1) << expected) < (0 + header)) {
    expected++;
  }
  TEST_ASSERT_EQUAL_UINT(expected, btok(0));
}

void test_btok_small_value(void) {
  size_t header = sizeof(struct avail);
  int expected = SMALLEST_K;
  while ((UINT64_C(1) << expected) < (1 + header)) {
    expected++;
  }
  TEST_ASSERT_EQUAL_UINT(expected, btok(1));
}

void test_btok_exact_boundary(void) {
  size_t header = sizeof(struct avail);
  int k_value = 8;
  // Calculate bytes so that bytes + header equals 2^k_value.
  size_t bytes = (UINT64_C(1) << k_value) - header;
  TEST_ASSERT_EQUAL_UINT(k_value, btok(bytes));
}

void test_btok_just_above_boundary(void) {
  size_t header = sizeof(struct avail);
  int k_value = 8;
  // This makes totalSize equal to 2^k_value + 1.
  size_t bytes = (UINT64_C(1) << k_value) - header + 1;
  TEST_ASSERT_EQUAL_UINT(k_value + 1, btok(bytes));
}

void test_btok_large_value(void) {
  size_t header = sizeof(struct avail);
  int expected = SMALLEST_K;
  size_t testBytes = 10000; // Arbitrary larger value
  while ((UINT64_C(1) << expected) < (testBytes + header)) {
    expected++;
  }
  TEST_ASSERT_EQUAL_UINT(expected, btok(testBytes));
}

void test_buddy_calc_basic(void) {
  // Allocate a memory buffer to simulate the buddy pool.
  size_t pool_size = 1024; // Must be large enough to hold both buddy blocks.
  void *poolMem = malloc(pool_size);
  TEST_ASSERT_NOT_NULL(poolMem);

  // Set up the buddy_pool structure.
  struct buddy_pool pool;
  pool.base = poolMem;

  // Use k = 8, so block size is 256 bytes.
  int k_val = 8;
  size_t blockSize = UINT64_C(1) << k_val; // 256

  // Create two buddy blocks:
  // blockA at offset 0 and blockB at offset blockSize.
  struct avail *blockA = (struct avail *)poolMem;
  struct avail *blockB = (struct avail *)((char *)poolMem + blockSize);

  // Initialize both blocks.
  blockA->kval = k_val;
  blockB->kval = k_val;
  blockA->tag = BLOCK_AVAIL;
  blockB->tag = BLOCK_AVAIL;

  // When calculating the buddy of blockA, we should get blockB.
  struct avail *calculatedBuddyA = buddy_calc(&pool, blockA);
  TEST_ASSERT_EQUAL_PTR(blockB, calculatedBuddyA);

  // Similarly, buddy_calc on blockB should return blockA.
  struct avail *calculatedBuddyB = buddy_calc(&pool, blockB);
  TEST_ASSERT_EQUAL_PTR(blockA, calculatedBuddyB);

  free(poolMem);
}

void test_buddy_calc_unavailable(void) {
  // Allocate a memory buffer to simulate the buddy pool.
  size_t pool_size = 1024; // Ensure the buffer is large enough.
  void *poolMem = malloc(pool_size);
  TEST_ASSERT_NOT_NULL(poolMem);

  // Set up the buddy_pool structure.
  struct buddy_pool pool;
  pool.base = poolMem;

  // Again, use k = 8 for a block size of 256 bytes.
  int k_val = 8;
  size_t blockSize = UINT64_C(1) << k_val; // 256

  // Create two buddy blocks.
  struct avail *blockA = (struct avail *)poolMem;
  struct avail *blockB = (struct avail *)((char *)poolMem + blockSize);

  // Initialize the blocks, but mark blockB as not available.
  blockA->kval = k_val;
  blockB->kval = k_val;
  blockA->tag = BLOCK_AVAIL;
  blockB->tag = BLOCK_RESERVED; // Simulate that the buddy is not free.

  // Since blockB is not available, buddy_calc should return NULL for blockA.
  struct avail *calculatedBuddyA = buddy_calc(&pool, blockA);
  TEST_ASSERT_NULL(calculatedBuddyA);

  free(poolMem);
}

void test_buddy_calc_symmetry(void) {
  // Allocate a pool large enough for two blocks.
  size_t poolSize = UINT64_C(1) << 10; // 1024 bytes
  void *poolMem = malloc(poolSize);
  TEST_ASSERT_NOT_NULL(poolMem);

  struct buddy_pool pool;
  pool.base = poolMem;

  int k_val = 8; // Block size will be 2^8 = 256 bytes.
  size_t blockSize = UINT64_C(1) << k_val;

  // Set up two buddy blocks:
  struct avail *blockA = (struct avail *)poolMem;
  struct avail *blockB = (struct avail *)((char *)poolMem + blockSize);

  // Initialize both blocks.
  blockA->kval = k_val;
  blockB->kval = k_val;
  blockA->tag = BLOCK_AVAIL;
  blockB->tag = BLOCK_AVAIL;

  // Compute buddy of blockA.
  struct avail *buddyA = buddy_calc(&pool, blockA);
  TEST_ASSERT_NOT_NULL(buddyA);
  // Then, compute buddy of buddyA. It should be blockA.
  struct avail *buddyOfBuddy = buddy_calc(&pool, buddyA);
  TEST_ASSERT_EQUAL_PTR(blockA, buddyOfBuddy);

  free(poolMem);
}

void test_buddy_calc_multiple_k(void) {
  for (int k_val = SMALLEST_K; k_val <= 10; k_val++) {
    size_t blockSize = UINT64_C(1) << k_val;
    // Allocate a pool that can hold two blocks of the given size.
    size_t poolSize = blockSize * 2;
    void *poolMem = malloc(poolSize);
    TEST_ASSERT_NOT_NULL(poolMem);

    struct buddy_pool pool;
    pool.base = poolMem;

    struct avail *blockA = (struct avail *)poolMem;
    struct avail *blockB = (struct avail *)((char *)poolMem + blockSize);

    blockA->kval = k_val;
    blockB->kval = k_val;
    blockA->tag = BLOCK_AVAIL;
    blockB->tag = BLOCK_AVAIL;

    // Verify that buddy_calc returns the correct buddy.
    struct avail *calcBuddyA = buddy_calc(&pool, blockA);
    TEST_ASSERT_EQUAL_PTR(blockB, calcBuddyA);
    struct avail *calcBuddyB = buddy_calc(&pool, blockB);
    TEST_ASSERT_EQUAL_PTR(blockA, calcBuddyB);

    free(poolMem);
  }
}

void test_buddy_calc_unavailable_case(void) {
  size_t poolSize = UINT64_C(1) << 8; // e.g., 256 bytes * 2 = 512 bytes.
  void *poolMem = malloc(poolSize);
  TEST_ASSERT_NOT_NULL(poolMem);

  struct buddy_pool pool;
  pool.base = poolMem;

  int k_val = 7; // Block size will be 2^7 = 128 bytes.
  size_t blockSize = UINT64_C(1) << k_val;

  struct avail *blockA = (struct avail *)poolMem;
  struct avail *blockB = (struct avail *)((char *)poolMem + blockSize);

  blockA->kval = k_val;
  blockB->kval = k_val;
  blockA->tag = BLOCK_AVAIL;
  blockB->tag = BLOCK_RESERVED; // Buddy is not available.

  // Since blockB is not available, buddy_calc should return NULL for blockA.
  struct avail *result = buddy_calc(&pool, blockA);
  TEST_ASSERT_NULL(result);

  free(poolMem);
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
 * Test allocating 1 byte to make sure we split the blocks all the way down
 * to MIN_K size. Then free the block and ensure we end up with a full
 * memory pool again
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

/**
 * Tests the allocation of one massive block that should consume the entire
 * memory pool and makes sure that after the pool is empty we correctly fail
 * subsequent calls.
 */
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

  // Verify that a call on an empty tool fails as expected and errno is set to
  // ENOMEM.
  void *fail = buddy_malloc(&pool, 5);
  assert(fail == NULL);
  assert(errno = ENOMEM);

  // Free the memory and then check to make sure everything is OK
  buddy_free(&pool, mem);
  check_buddy_pool_full(&pool);
  buddy_destroy(&pool);
}

void test_malloc_zero_size(void) {
  struct buddy_pool pool;
  buddy_init(&pool, 1 << MIN_K);
  // Requesting zero bytes should return NULL.
  void *ptr = buddy_malloc(&pool, 0);
  TEST_ASSERT_NULL(ptr);
  buddy_destroy(&pool);
}

void test_malloc_free_small(void) {
  // Allocate a small block, then free it.
  struct buddy_pool pool;
  buddy_init(&pool, 1 << MIN_K);

  size_t req = 100; // A small allocation request.
  void *ptr = buddy_malloc(&pool, req);
  TEST_ASSERT_NOT_NULL(ptr);

  // Free the block and check that the entire pool is merged back.
  buddy_free(&pool, ptr);
  check_buddy_pool_full(&pool);

  buddy_destroy(&pool);
}

void test_malloc_free_large(void) {
  // This test allocates a block that (with header) exactly equals half the
  // pool, triggering the special-case that consumes the entire pool.
  struct buddy_pool pool;
  buddy_init(&pool, 1 << MIN_K);

  // Allocate a block whose total size (request+header) equals 1 << MIN_K.
  size_t ask = (1 << MIN_K) - sizeof(struct avail);
  void *ptr = buddy_malloc(&pool, ask);
  TEST_ASSERT_NOT_NULL(ptr);

  // The allocation should remove the entire free block, so the free lists
  // should be empty.
  check_buddy_pool_empty(&pool);

  // Free the block; the pool should merge back into a full state.
  buddy_free(&pool, ptr);
  check_buddy_pool_full(&pool);

  buddy_destroy(&pool);
}

void test_multiple_allocations_and_frees(void) {
  // Allocate several blocks, free them in a non-sequential order,
  // and then ensure that the buddy system properly merges all free blocks.
  struct buddy_pool pool;
  // Use a larger pool than the minimum so that splitting occurs.
  buddy_init(&pool, 1 << (MIN_K + 1));

  void *ptr1 = buddy_malloc(&pool, 200);
  void *ptr2 = buddy_malloc(&pool, 300);
  void *ptr3 = buddy_malloc(&pool, 400);

  TEST_ASSERT_NOT_NULL(ptr1);
  TEST_ASSERT_NOT_NULL(ptr2);
  TEST_ASSERT_NOT_NULL(ptr3);

  // Free blocks in mixed order.
  buddy_free(&pool, ptr2);
  buddy_free(&pool, ptr1);
  buddy_free(&pool, ptr3);

  // After freeing, the entire pool should be merged.
  check_buddy_pool_full(&pool);

  buddy_destroy(&pool);
}

void test_random_alloc_free(void) {
  struct buddy_pool pool;
  buddy_init(&pool, 1 << (MIN_K + 2)); // Larger pool for more allocations
  const int iterations = 100;
  void *allocated[50] = {0};
  int alloc_count = 0;
  srand(42); // Fixed seed for reproducibility

  for (int i = 0; i < iterations; i++) {
    int op = rand() % 2;
    if (op == 0) { // allocate
      size_t size = (rand() % 100) + 1;
      void *p = buddy_malloc(&pool, size);
      if (p != NULL) {
        allocated[alloc_count++] = p;
        if (alloc_count >= 50)
          alloc_count = 49; // prevent overflow
      }
    } else { // free if available
      if (alloc_count > 0) {
        int idx = rand() % alloc_count;
        buddy_free(&pool, allocated[idx]);
        allocated[idx] = allocated[alloc_count - 1];
        allocated[alloc_count - 1] = NULL;
        alloc_count--;
      }
    }
  }
  // Free any remaining allocations.
  for (int i = 0; i < alloc_count; i++) {
    buddy_free(&pool, allocated[i]);
  }

  check_buddy_pool_full(&pool);
  buddy_destroy(&pool);
}

void test_repeated_alloc_free(void) {
  struct buddy_pool pool;
  buddy_init(&pool, 1 << (MIN_K + 1));

  for (int i = 0; i < 100; i++) {
    void *p = buddy_malloc(&pool, 200);
    TEST_ASSERT_NOT_NULL(p);
    buddy_free(&pool, p);
    check_buddy_pool_full(&pool);
  }

  buddy_destroy(&pool);
}

/**
 * Tests to make sure that the struct buddy_pool is correct and all fields
 * have been properly set kval_m, avail[kval_m], and base pointer after a
 * call to init
 */
void test_buddy_init(void) {
  fprintf(stderr, "->Testing buddy init\n");
  // Loop through all kval MIN_k-DEFAULT_K and make sure we get the correct
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

int main(void) {
  time_t t;
  unsigned seed = (unsigned)time(&t);
  fprintf(stderr, "Random seed:%d\n", seed);
  srand(seed);
  printf("Running memory tests.\n");

  UNITY_BEGIN();
  RUN_TEST(test_buddy_init);
  RUN_TEST(test_btok_zero);
  RUN_TEST(test_btok_small_value);
  RUN_TEST(test_btok_exact_boundary);
  RUN_TEST(test_btok_just_above_boundary);
  RUN_TEST(test_btok_large_value);
  RUN_TEST(test_buddy_calc_basic);
  RUN_TEST(test_buddy_calc_unavailable);
  RUN_TEST(test_buddy_calc_symmetry);
  RUN_TEST(test_buddy_calc_multiple_k);
  RUN_TEST(test_buddy_malloc_one_byte);
  RUN_TEST(test_buddy_malloc_one_large);
  RUN_TEST(test_buddy_calc_unavailable_case);
  RUN_TEST(test_malloc_zero_size);
  RUN_TEST(test_malloc_free_small);
  RUN_TEST(test_malloc_free_large);
  RUN_TEST(test_multiple_allocations_and_frees);
  RUN_TEST(test_random_alloc_free);
  RUN_TEST(test_repeated_alloc_free);
  RUN_TEST(test_pool_initial_state);
  RUN_TEST(test_pool_after_allocations);
  RUN_TEST(test_total_free_memory_calculation);
  return UNITY_END();
}
