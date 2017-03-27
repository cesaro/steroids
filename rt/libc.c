
#include <unistd.h>
#include "pthread.h"

// this file will be #include'd in the main.c file

#define ALIGN16(i) (((uint64_t) (i)) & 0xf ? (((uint64_t) (i)) + 16) & -16ul : (uint64_t) (i))

static int __rt_errno = 0;
// unistd.h

static uint8_t *__malloc_ptr;

void __rt_libc_init ()
{
   __rt_mm_init ();
   __rt_thread_init ();
}

void __rt_libc_term ()
{
   __rt_thread_term ();
   __rt_mm_term ();
}

void __rt_mm_init ()
{
   //printf ("stid: rt: mm: initializing memory manager\n");
   __malloc_ptr = rt->heap.begin;
}

void __rt_mm_term ()
{
   //printf ("stid: rt: mm: terminating memory manager\n");
}

void *_rt_calloc  (size_t n, size_t size)
{
   return _rt_malloc (n * size);
}

void *_rt_malloc_uninitialized  (size_t size)
{
   void *ptr;

   if (size == 0) return 0;
   // get a free memory area, if any memory is left in the heap
   ptr = __malloc_ptr;
   __malloc_ptr = (uint8_t*) ALIGN16 (__malloc_ptr + size);
   if (__malloc_ptr > rt->heap.end)
   {
      __malloc_ptr = ptr;
      ptr = 0;
      PRINT ("out of memory: __malloc_ptr %p size %lu, returning null",
            __malloc_ptr, size);
   }

   //printf ("stid: rt: malloc: ret %p size %zu\n", ptr, size);
   //TRACE2 (RT_MALLOC, ptr, size);
   return ptr;
}

void *_rt_malloc  (size_t size)
{
   void *ptr;
   ptr = _rt_malloc_uninitialized (size);
   if (ptr) memset (ptr, 0, size);
   return ptr;
}

void _rt_free (void *ptr)
{
   //TRACE1 (RT_FREE, ptr);
   //printf ("stid: rt: free: ptr %p", ptr);
   (void) ptr; 
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
   newptr = _rt_malloc (size);
   //printf ("stid: rt: realloc: ptr %p newptr %p size %zu", ptr, newptr, size);
   if (!newptr) return 0;

   // this is strictly not safe, as size could be larger than the size of the
   // area pointed by ptr and then the memory areas could overlap
   PRINT ("warning: performing unsafe realloc: ptr %p newptr %p size %lu",
         ptr, newptr, size);
   memcpy (newptr, ptr, size);
   _rt_free (ptr);
   return newptr;
}

void (*__rt_atexit_table[RT_MAX_ATEXIT_FUNS]) (void);
int __rt_atexit_size = 0;

int _rt_atexit (void (* fun) (void))
{
   printf ("stid: rt: atexit: fun %p\n", fun);
   ASSERT (__rt_atexit_size < RT_MAX_ATEXIT_FUNS);
   __rt_atexit_table[__rt_atexit_size] = fun;
   __rt_atexit_size++;
   return 0;
}

void _rt_exit (int status)
{
   int i;

   _printf ("stid: rt: exit: status %d\n", status);

   // call atexit(3) functions
   for (i = __rt_atexit_size - 1; i >= 0; i--) __rt_atexit_table[i] ();
   _rt__exit (status);
}

void _rt_abort ()
{
   int ret;

   // FIXME - factorize common code here and in _rt__exit
   // the conditional variable, etc...
   printf ("stid: rt: abort: called from t%d\n", TID (__state.current));
   if (TID (__state.current) != 0)
   {
      printf ("stid: rt: abort: not supported, this is a limitation of steroids.\n");
      ASSERT (0);
      __rt_panic ();
   }

   // destroy conditional variable
   ret = pthread_cond_destroy (&__state.tcbs[0].cond);
   if (ret)
   {
      PRINT ("t0: cond var: errors while destroying: %s; ignoring",
            strerror (ret));
   }

   TRACE0 (RT_ABORT);

   // we return control immediately to the host
   __rt_cend (253);
}

void _rt__exit (int status)
{
   int ret;

   _printf ("stid: rt: _exit: status %d\n", status);

   fflush (stdout);
   fflush (stderr);

   // we do not support exiting from another thread != main
   if (TID (__state.current) != 0)
   {
      PRINT ("t%d: called exit: not currently supported by the runtime",
            TID (__state.current) != 0);
      ASSERT (0);
      __rt_panic ();
   }

   // log the EXIT event for the calling thread (will be main at this point)
   breakme ();
   TRACE0 (RT_THEXIT);
   rt->trace.num_blue[0]++;

   // issue a warning if the status is non zero
   if (status)
      TRACE3 (RT_EXITNZ, status);

   // consume one event (the last) from the replay sequence
   ASSERT (rt->replay.current->count == 1 || rt->replay.current->count == -1);
   if (rt->replay.current->count == 1) rt->replay.current->count = 0;

   // destroy conditional variable
   ret = pthread_cond_destroy (&__state.tcbs[0].cond);
   if (ret)
   {
      PRINT ("t0: cond var: errors while destroying: %s; ignoring",
            strerror (ret));
   }

   // return control to the host
   __rt_cend (status);
}

unsigned int _rt_sleep (unsigned int sec)
{
#if 1
   PRINT ("t%d: sleep: sec %u, returning EINTR immediately",
         TID (__state.current), sec);
   return EINTR;
#else
   // could we choose another thread to schedule here?
   return sleep (sec);
#endif
}

int _rt_usleep (useconds_t us)
{
#if 1
   PRINT ("t%d: usleep: usec %u, returning EINTR immediately",
         TID (__state.current), us);
   return EINTR;
#else
   // could we choose another thread to schedule here?
   return usleep (us);
#endif
}

int *_rt___errno_location ()
{
   __rt_errno = *__errno_location (); // in glibc !!
   _printf ("stid: rt: errno_location: called\n");
   return &__rt_errno;
}

// stdin
extern inline FILE *__rt_var_load_stdin () {
   return stdin;
}

extern inline void __rt_var_store_stdin (FILE *f)
{
   stdin = f;
}

// stdout
extern inline FILE *__rt_var_load_stdout ()
{
   return stdout;
}

extern inline void __rt_var_store_stdout (FILE *f)
{
   stdout = f;
}

// stderr
extern inline FILE *__rt_var_load_stderr ()
{
   return stderr;
}

extern inline void __rt_var_store_stderr (FILE *f)
{
   stderr = f;
}

// optarg
extern inline char *__rt_var_load_optarg ()
{
   return optarg;
}

extern inline void __rt_var_store_optarg (char *c)
{
   optarg = c;
}

// optind
extern inline int __rt_var_load_optind ()
{
   return optind;
}

extern inline void __rt_var_store_optind (int i)
{
   optind = i;
}

// opterr
extern inline int __rt_var_load_opterr ()
{
   return opterr;
}

extern inline void __rt_var_store_opterr (int i)
{
   opterr = i;
}

// optopt
extern inline int __rt_var_load_optopt ()
{
   return optopt;
}

extern inline void __rt_var_store_optopt (int i)
{
   optopt = i;
}

extern inline char* __rt_var_load_program_invocation_name ()
{
   return program_invocation_name;
}
extern inline void __rt_var_store_program_invocation_name (char *n)
{
   program_invocation_name = n;
}
extern inline char* __rt_var_load_program_invocation_short_name ()
{
   return program_invocation_short_name;
}
extern inline void __rt_var_store_program_invocation_short_name (char *n)
{
   program_invocation_short_name = n;
}

int _rt_fclose (FILE *f)
{
   const char *n =
         f == stdout ? "stdout" :
         f == stdin ? "stdin" :
         f == stderr ? "stderr" : "?";
   if (f == stdout || f == stdin || f == stderr)
   {
      PRINT ("f %p (%s): refraining to close %s, but returning success\n",
            f, n, n);
      return 0; // fake!
   }
   return fclose (f);
}

int _rt_close (int fd)
{
   if (fd == 0 || fd == 1 || fd == 2)
   {
      PRINT ("fd %d: refraining to close %d, but returning success\n", fd, fd);
      return 0; // fake !
   }
   return close (fd);
}

void _rt___assert_fail (const char *__assertion, const char *__file,
			   unsigned int __line, const char *__function)
{
   printf ("stid: rt: assert-fail: called!\n");
   printf ("%s:%d: %s: Assertion `%s' failed. Aborting.\n",
         __file, __line, __function, __assertion);
   _rt_abort ();
}
