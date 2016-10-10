
// this file will be #include'd in the main.c file

static uint64_t __malloc_ptr;

void _rt_mm_init ()
{
   __malloc_ptr = memstart;
}

void *malloc  (size_t size)
{
   void *ptr;
   ptr = (void*) __malloc_ptr;
   __malloc_ptr += -16llu & (size + 16);
   printf ("stid: rt: malloc: size %zu ret %p\n", size, ptr);
   return ptr;
}

void  _rt_free    (void *ptr)
{
   uint64_t s;
   printf ("stid: rt: free: ptr %p\n", ptr);
   printf ("stid: rt: free: memstart %p\n", (void*) memstart);
   printf ("stid: rt: free: memend   %p\n", (void*) memend);
   s = memend - memstart;
   printf ("stid: rt: free: %luB (%luM)\n", s, s / (1024 * 1024));
   printf ("stid: rt: free: rt->trace.evstart %p\n", (void*) rt->trace.evstart);
   printf ("stid: rt: free: rt->trace.evptr   %p\n", (void*) rt->trace.evptr);
   printf ("stid: rt: free: rt->trace.evend   %p\n", (void*) rt->trace.evend);
   s = rt->trace.evend - rt->trace.evstart;
   printf ("stid: rt: free: %lu ev (%lu Mev)\n", s, s / (1024 * 1024));
   //return free (ptr);
}

void *_rt_realloc (void *ptr, size_t size)
{
   //void *ret;
   //ret = realloc (ptr, size);
   printf ("stid: rt: malloc: ptr %p size %zu ret %p\n", ptr, size, NULL);
   return 0;
}
