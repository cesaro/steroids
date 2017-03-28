
#include <sys/types.h>

void __rt_mm_init ();
void __rt_mm_term ();

// these two are for internal use
void *__rt_malloc_uninitialized  (size_t size);
void *__rt_malloc_initialized  (size_t size);
void __rt_free_iternal (void *ptr);

void *_rt_malloc  (size_t size);
void *_rt_calloc  (size_t n, size_t size);
void _rt_free (void *ptr);
void *_rt_realloc (void *ptr, size_t size);

#define ALIGN16(i) (((uint64_t) (i)) & 0xf ? (((uint64_t) (i)) + 16) & -16ul : (uint64_t) (i))

