
// this file will be #include'd in the main.c file

#define ALIGN16(i) (((uint64_t) (i)) & 0xf ? (((uint64_t) (i)) + 16) & -16ul : (uint64_t) (i))

static uint64_t __malloc_ptr;

void _rt_mm_init ()
{
   __malloc_ptr = (uint64_t) rt->heap.begin;
}

void *calloc  (size_t n, size_t size)
{
   return malloc (n * size);
}

void *malloc  (size_t size)
{
   void *ptr;
   ptr = (void*) __malloc_ptr;
   __malloc_ptr = ALIGN16 (__malloc_ptr + size);
   //printf ("stid: rt: malloc: ret %p size %zu\n", ptr, size);
   TRACE2 (MLLO, ptr, size);
   memset (ptr, 0, size); // necessary for repeatable execution!!!!
   return ptr;
}

void free (void *ptr)
{
   TRACE1 (FREE, ptr);
   //printf ("stid: rt: free: ptr %p", ptr);
   (void) ptr; 
}

void *realloc (void *ptr, size_t size)
{
   void *newptr;

   if (! ptr) return malloc (size);
   if (size == 0)
   {
      free (ptr);
      return 0;
   }
   newptr = malloc (size);
   //printf ("stid: rt: realloc: ptr %p newptr %p size %zu", ptr, newptr, size);
   if (!newptr) return 0;

   // this is strictly not safe, as size could be larger than the size of the
   // area pointed by ptr and then the memory areas could overlap
   memcpy (newptr, ptr, size);
   free (ptr);
   return newptr;
}
