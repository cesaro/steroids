
#include <unistd.h>
#include "pthread.h"

// this file will be #include'd in the main.c file

#define ALIGN16(i) (((uint64_t) (i)) & 0xf ? (((uint64_t) (i)) + 16) & -16ul : (uint64_t) (i))

static int __rt_errno = 0;
// unistd.h

static pthread_mutex_t mutex_malloc = PTHREAD_MUTEX_INITIALIZER;

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

   // get a free memory area, if any memory is left in the heap
   pthread_mutex_lock (&mutex_malloc);
   ptr = __malloc_ptr;
   __malloc_ptr = (uint8_t*) ALIGN16 (__malloc_ptr + size);
   if (__malloc_ptr > rt->heap.end)
   {
      __malloc_ptr = ptr;
      ptr = 0;
      PRINT ("out of memory: __malloc_ptr %p size %lu, returning null",
            __malloc_ptr, size);
   }
   pthread_mutex_unlock (&mutex_malloc);

   //printf ("stid: rt: malloc: ret %p size %zu\n", ptr, size);
   TRACE2 (RT_MALLOC, ptr, size);
   return ptr;
}

void *_rt_malloc  (size_t size)
{
   void *ptr;
   ptr = _rt_malloc_uninitialized (size);
   if (ptr) memset (ptr, 0, size); // necessary for repeatable execution!!!!
   return ptr;
}

void _rt_free (void *ptr)
{
   TRACE1 (RT_FREE, ptr);
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
   // FIXME - we should check that we are called from main, we should destroy
   // the conditional variable, etc...
   printf ("stid: rt: abort: called from t%d\n", TID (__rt_thst.current));
   if (TID (__rt_thst.current) != 0)
   {
      printf ("stid: rt: abort: not supported, this is a limitation of steroids.\n");
      ASSERT (0);
   }
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
   if (TID (__rt_thst.current) != 0)
   {
      PRINT ("t%d: called exit: not currently supported by the runtime",
            TID (__rt_thst.current) != 0);
      ASSERT (0);
      __rt_panic ();
   }

   // log the EXIT event for the calling thread (will be main at this point)
   TRACE0 (RT_THEXIT);
   rt->trace.num_blue[0]++;

   // consume one event (the last) from the replay sequence
   ASSERT (*rt->replay.current == 1 || *rt->replay.current == -1);
   if (*rt->replay.current == 1) *rt->replay.current = 0;

   // destroy conditional variable
   ret = pthread_cond_destroy (&__rt_thst.tcbs[0].cond);
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
   //struct rt_tcb *me = __rt_thst.current;
   unsigned ret;

   //__rt_thread_protocol_yield (me);
   ret = sleep (sec);
   //__rt_thread_protocol_wait (me);
   return ret;
}

int _rt_usleep (useconds_t us)
{
   //struct rt_tcb *me = __rt_thst.current;
   unsigned ret;

   //__rt_thread_protocol_yield (me);
   ret = usleep (us);
   //__rt_thread_protocol_wait (me);
   return ret;
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
   printf ("stid: rt: fclose: f %p (%s)\n",
         f,
         f == stdout ? "stdout" :
         f == stdin ? "stdin" :
         f == stderr ? "stderr" : "?");
   fflush (stdout);
   if (f == stdout || f == stdin || f == stderr) return 0; // fake!
   return fclose (f);
}

int _rt_close (int fd)
{
   printf ("stid: rt: close: fd %d\n", fd);
   fflush (stdout);
   if (fd == 0 || fd == 1 || fd == 2) return 0; // fake !
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
