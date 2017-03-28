
#include "proc.h"
#include "pthread.h"

void (*__rt_atexit_table[RT_MAX_ATEXIT_FUNS]) (void);
int __rt_atexit_size = 0;


void _rt_exit (int status)
{
   int i;

   STRACE (proc, "exit (c=%d)", status);

   // call atexit(3) functions
   for (i = __rt_atexit_size - 1; i >= 0; i--) __rt_atexit_table[i] ();
   _rt__exit (status);
}

void _rt_abort ()
{
   // FIXME - abort should terminate all threads right now, but we resort to
   // pthread_exit because we don't have currently any better alternative

   STRACE (proc, "abort()");

   ALERT ("Function abort() was called, but current support for abort() is very limited");
   ALERT ("Resorting to pthread_exit(), you might experience unexpected errors from this point on");

   TRACE0 (RT_ABORT);
   _rt_pthread_exit (0);
}

void _rt__exit (int status)
{
   int ret;

   STRACE (proc, "_exit (c=%d)", status);

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

int _rt_atexit (void (* fun) (void))
{
   STRACE (proc, "_exit (fun=%p)", fun);
   ASSERT (__rt_atexit_size < RT_MAX_ATEXIT_FUNS);
   __rt_atexit_table[__rt_atexit_size] = fun;
   __rt_atexit_size++;
   return 0;
}

