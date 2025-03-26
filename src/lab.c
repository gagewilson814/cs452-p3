#include "lab.h"
#include <errno.h>

size_t btok(size_t bytes) {}

struct avail *buddy_calc(struct buddy_pool *pool, struct avail *buddy) {}

void *buddy_malloc(struct buddy_pool *pool, size_t size) {}

void buddy_free(struct buddy_pool *pool, void *ptr) {}

void *buddy_realloc(struct buddy_pool *pool, void *ptr, size_t size) {}

void buddy_init(struct buddy_pool *pool, size_t size) {}

void buddy_destroy(struct buddy_pool *pool) {}
