
// this file will be #include'd in the main.c file

static uint64_t __malloc_ptr;

void _rt_mm_init ()
{
   __malloc_ptr = (uint64_t) rt->heap.begin;
}

void *malloc  (size_t size)
{
   void *ptr;
   ptr = (void*) __malloc_ptr;
   __malloc_ptr += -16llu & (size + 16);
   printf ("stid: rt: malloc: ret %p size %zu\n", ptr, size);
   return ptr;
}

void  _rt_free    (void *ptr)
{
   (void) ptr; 
   //uint64_t s;
   //return free (ptr);
}

void *_rt_realloc (void *ptr, size_t size)
{
   //void *ret;
   //ret = realloc (ptr, size);
   printf ("stid: rt: malloc: ptr %p size %zu ret %p\n", ptr, size, NULL);
   return 0;
}
