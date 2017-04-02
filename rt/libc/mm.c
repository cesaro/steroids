
#include "mm.h"

static uint8_t *__malloc_ptr;


void __rt_mm_init ()
{
   //printf ("stid: rt: mm: initializing memory manager\n");
   __malloc_ptr = rt->heap.begin;
}

void __rt_mm_term ()
{
   //printf ("stid: rt: mm: terminating memory manager\n");
}


void *__rt_malloc_uninitialized  (size_t size)
{
   void *ptr;

   // GLIBC returns a pointer to addressable memory, we return a pointer to
   // unaddressable memory; if the program uses it we should segfault
   if (size == 0) return (void*) 0x10;

   // get a free memory area, if any memory is left in the heap
   ptr = __malloc_ptr;
   __malloc_ptr = (uint8_t*) ALIGN16 (__malloc_ptr + size);
   if (__malloc_ptr > rt->heap.end)
   {
      __malloc_ptr = ptr;
      ptr = 0;
      ALERT ("out of memory: __malloc_ptr %p size %lu, returning null",
            __malloc_ptr, size);
   }

   //printf ("stid: rt: malloc: ret %p size %zu\n", ptr, size);
   //TRACE2 (RT_MALLOC, ptr, size);
   return ptr;
}

void *__rt_malloc_initialized  (size_t size)
{
   void *ptr;
   ptr = __rt_malloc_uninitialized (size);
   if (ptr) memset (ptr, 0, size);
   return ptr;
}

void __rt_free_internal (void *ptr)
{
   //TRACE1 (RT_FREE, ptr);
   (void) ptr;
}

void *_rt_malloc  (size_t size)
{
   void *ptr;
   ptr = __rt_malloc_initialized (size);
   STRACE (malloc, "malloc (size=%zu) = %p", size, ptr);
   return ptr;
}

void *_rt_calloc  (size_t n, size_t size)
{
   void *ptr;
   ptr = __rt_malloc_initialized (n * size);
   STRACE (malloc, "calloc (n=%zu, size=%zu) = %p", n, size, ptr);
   return ptr;
}

void _rt_free (void *ptr)
{
   STRACE (malloc, "free (ptr=%p)", ptr);
   __rt_free_internal (ptr);
}

void *_rt_realloc (void *ptr, size_t size)
{
   void *newptr;

   if (! ptr) return _rt_malloc (size);
   if (size == 0)
   {
      _rt_free (ptr);
      return 0;
   }
   newptr = __rt_malloc_uninitialized (size);
   //printf ("stid: rt: realloc: ptr %p newptr %p size %zu", ptr, newptr, size);
   if (!newptr) return 0;

   // this is strictly not safe, as size could be larger than the size of the
   // area pointed by ptr and then the memory areas could overlap
   memcpy (newptr, ptr, size);
   __rt_free_internal (ptr);
   STRACE (malloc, "realloc (ptr=%p, size=%zu) = %p", ptr, size, newptr);
   return newptr;
}

