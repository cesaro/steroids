
#include <unistd.h>
#include "pthread.h"

// this file will be #include'd in the main.c file

#define ALIGN16(i) (((uint64_t) (i)) & 0xf ? (((uint64_t) (i)) + 16) & -16ul : (uint64_t) (i))

int _rt_errno = 0;
// unistd.h

static uint64_t __malloc_ptr;

void _rt_libc_init ()
{
   _rt_mm_init ();
   _rt_thread_init ();
}

void _rt_libc_term ()
{
   _rt_thread_term ();
   _rt_mm_term ();
}

void _rt_mm_init ()
{
   printf ("stid: rt: mm: initializing memory manager\n");
   __malloc_ptr = (uint64_t) rt->heap.begin;
}

void _rt_mm_term ()
{
   printf ("stid: rt: mm: terminating memory manager\n");
}

void *_rt_calloc  (size_t n, size_t size)
{
   return _rt_malloc (n * size);
}

void *_rt_malloc  (size_t size)
{
   void *ptr;
   ptr = (void*) __malloc_ptr;
   __malloc_ptr = ALIGN16 (__malloc_ptr + size);
   //printf ("stid: rt: malloc: ret %p size %zu\n", ptr, size);
   TRACE2 (RT_MALLOC, ptr, size);
   memset (ptr, 0, size); // necessary for repeatable execution!!!!
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

void (*_rt_atexit_table[RT_MAX_ATEXIT_FUNS]) (void);
int _rt_atexit_size = 0;

int _rt_atexit (void (* fun) (void))
{
   printf ("stid: rt: atexit: fun %p\n", fun);
   ASSERT (_rt_atexit_size < RT_MAX_ATEXIT_FUNS);
   _rt_atexit_table[_rt_atexit_size] = fun;
   _rt_atexit_size++;
   return 0;
}

void _rt_exit (int status)
{
   int i;

   printf ("stid: rt: exit: status %d\n", status);

   DEBUG ("hereeeeee");
   // call atexit(3) functions
   for (i = _rt_atexit_size - 1; i >= 0; i--) _rt_atexit_table[i] ();
   DEBUG ("and heeeeeeeeeereeeeee");
   _rt__exit (status);
}

void _rt_abort ()
{
   // FIXME - we should check that we are called from main, we should destroy
   // the conditional variable, etc...
   printf ("stid: rt: abort: called!!!!!\n");
   // we return control immediately to the host
   _rt_cend (253);
}

void _rt__exit (int status)
{
   int ret;

   printf ("stid: rt: _exit: status %d\n", status);

   fflush (stdout);
   fflush (stderr);

   // we do not support exiting from another thread != main
   if (TID (__rt_thst.current) != 0)
   {
      PRINT ("t%d: called exit: not currently supported by the runtime",
            TID (__rt_thst.current) != 0);
      ASSERT (0);
      _rt_panic ();
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
   _rt_cend (status);
}

unsigned int _rt_sleep (unsigned int sec)
{
   //struct rt_tcb *me = __rt_thst.current;
   unsigned ret;

   //_rt_thread_protocol_yield (me);
   ret = sleep (sec);
   //_rt_thread_protocol_wait (me);
   return ret;
}

int _rt_usleep (useconds_t us)
{
   //struct rt_tcb *me = __rt_thst.current;
   unsigned ret;

   //_rt_thread_protocol_yield (me);
   ret = usleep (us);
   //_rt_thread_protocol_wait (me);
   return ret;
}

int *_rt___errno_location ()
{
   _rt_errno = *__errno_location (); // in glibc !!
   printf ("stid: rt: errno_location: called\n");
   return &_rt_errno;
}

// stdin
extern inline FILE *_rt_var_load_stdin () {
   return stdin;
}

extern inline void _rt_var_store_stdin (FILE *f)
{
   stdin = f;
}

// stdout
extern inline FILE *_rt_var_load_stdout ()
{
   return stdout;
}

extern inline void _rt_var_store_stdout (FILE *f)
{
   stdout = f;
}

// stderr
extern inline FILE *_rt_var_load_stderr ()
{
   return stderr;
}

extern inline void _rt_var_store_stderr (FILE *f)
{
   stderr = f;
}

// optarg
extern inline char *_rt_var_load_optarg ()
{
   return optarg;
}

extern inline void _rt_var_store_optarg (char *c)
{
   optarg = c;
}

// optind
extern inline int _rt_var_load_optind ()
{
   return optind;
}

extern inline void _rt_var_store_optind (int i)
{
   optind = i;
}

// opterr
extern inline int _rt_var_load_opterr ()
{
   return opterr;
}

extern inline void _rt_var_store_opterr (int i)
{
   opterr = i;
}

// optopt
extern inline int _rt_var_load_optopt ()
{
   return optopt;
}

extern inline void _rt_var_store_optopt (int i)
{
   optopt = i;
}

extern inline char* _rt_var_load_program_invocation_name ()
{
   return program_invocation_name;
}
extern inline void _rt_var_store_program_invocation_name (char *n)
{
   program_invocation_name = n;
}
extern inline char* _rt_var_load_program_invocation_short_name ()
{
   return program_invocation_short_name;
}
extern inline void _rt_var_store_program_invocation_short_name (char *n)
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

