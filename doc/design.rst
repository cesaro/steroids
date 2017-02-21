
Design notes

Runtime
-------

- _rt_XXX functions are public, will hook functions named XXX
- __rt_* are private, the instrumenter will skip them, but they never substitute
  a call to _rt_* or something

Instrumentation
---------------

- events
- structure rt
- pointers instrumented in the module, pointing to the host's heap:
  static struct rt * const rt; // stored in the Executor object, in the host
  static const uint64_t memstart;
  static const uint64_t memend;
  static const uint64_t evend;

- format of the event stream; at least 1 available
- __rt_{store,load}_pre instrumented before the store; will stop execution if OOM fault
  and record one event
- then the store or the load
- then a call to __rt_{store,load}_post


To do
-----

- when the program does not contain all functions it calls and the remaining
  ones cannot be found in in the host process (or loaded libraries) the call to
  ee->finalizeObject() fails and I cannot catch the error

Guest stack switch
--------------

- Callee-saved: RBP, RBX, and R12–R15.
- All others must be saved by the caller if it wishes to preserve their values.

POSIX threads library
---------------------

- Thread/process termination
  We impose that only main can call exit(3).
  We impose that the last thread to finish must be main.
  So main must pthread_join() for any other thread.

- Stacks.
  We allow pthread_attr_{get,set}stack(addr)?, the addresses that we might
  receive have been allocated inside of the guest, so should be safe
  -> Por el momento, no hacemos free() de la pila

- Detached threads.
  For the time being, we do not support them.
  main() has to pthread_join all threads
  Question: can we return to the host from a thread different than main?

- Mutexes.
  We have to keep track of all mutexes currently alive to destroy them before
  the guest terminates.
  Also the mutexes and condvars in the library


Questions:
- when the application exit(3)'s, should it call pthread_cancel for every
  thread still alive?
- Can we return to the host from a thread different than main?
- How can we reclaim the stack from the same thread ??
- with the current implementation, how to detect a deadlock in the runtime?
- we make copy of the pointers stdio, stderr, and stdin inside the guest and
  initialize it the beginning, but this won't work if someones wants to modify
  the value of these pointers. How to deal in general with the problem of
  having variables in the libc that are accessible in the program?

  
continue here:
x how to insert main as the first thread
x how to prevent pthreads to call exit on the last thread
x improve support for arbitrary types on load/store
x show we generate events RD|WR for 1 bit ?
- when preserving data segments, we should only preserve initialized data
  segments, such as .data and .rodata, not .bss !!
- for those pthread functions that we do not handle, issue a warning in the
  stream


Notes
-----

- add a new ERROR event to send back errors discovered by the rt
- look at the possibility of implementing the threading rt with GNU pth:
  https://www.gnu.org/software/pth/pth-manual.html
- have a look to this white paper
  https://www.dre.vanderbilt.edu/~schmidt/PDF/DSIS_Chapter_Waddington.pdf
- Many example simple programs with pthreads:
  https://github.com/snikulov/prog_posix_threads
- Much easier to allow access to specific variables in the libc: white-list
  them in advance so that the Instrumenter does not generate callbacks for them
  and so you freely allow the thread to read/write from them!!!
- the isalnum(3) macros return a pointer to a table located in the libc, see
  <ctype.h>
- the protocol to control the replay and free mode is crap! we need to
  substitute this by a specialized pthreads library
- improve or remove the memory clearing strategy
- if a threads calls one of the functions that we instrumented (eg.,
  pthread_mutex_lock()) using a pointer to a function, that escapes to the
  control of steroids::

   int (* f) (pthread_mutex_t *m);
   f = pthread_mutex_lock;
   f (...); // !!


Limitation: the size the log trace cannot be greater than 2^31, as otherwise, at
the end of a replay sequence, the current.count coulds decrease 2^31 times,
overflow and become positive.


::
 csm:             mutex; // cs mutex
 replay:          vector of pairs {int tid, int count};
 current:         pointer to a pair {int tid, int count}
 state.tcbs:      vector of struct tcb {flags.{alive,detached}, cond}
 state.numalive   int
 state.current    tcb pointer

 Invariant:
 - replay.back().tid == -1 == FREEMODE;
 - replay.back().count == -1;

 // called from the main thread
 lib_init () :
   // wait_first()
   lock (csm);
   ASSERT (current.tid == FREEMODE or current.count >= 1)
   current.count--; // consume THSTART, if not in freemode

 // called from the main thread
 lib_term () :
   release (csm)
   destroy (csm)

 pthread_create (tid, attr, start, arg) :
   t = get from from state.tcbs
   initialize t
   allocate stack
   create NPTL thread (t, stack, etc)
   state.numalive++;
   LOG (THCREATE)
   current.count--;
   ASSERT (if current.count < 0 then either we are in freemode or this is the
            last context switch before the free mode)

 pthread_join (t, retval) :
   if (current.count >= 1)
      join (t, other)
   else
      yield (me)
      join (t, other)
      wait (me, NULL)
   LOG (THJOIN)
   ASSERT (current.count >= 1 or current.tid == FREEMODE)
   current.count--;

 thread_routine() :
   wait (me)
   current.count--;
   ret = usercode (arg)
   pthread_exit (ret)

 pthread_exit (arg) :
   if (me == 0) _rt_exit (0); // will do THEXIT
   LOG (THEXIT)
   state.numalive--
   current.count--
   ASSERT (either current.tid == FREEMODE, or this is the last context switch
         before the FREEMODE and current.count <= 0 or it is not the last
         context switch and current.count == 0)
   destroy me.cond
   yield (me)
   NTPL exit(me)

 wait (me, mut = NULL by default) :
   lock (csm)
   while (1)
      if (mut and current.tid == FREEMODE) return wait2 (me, mut)
      if (current.tid == FREEMODE or current.tid == me)
         if (state.current != me)
            LOG (THCTXSW, me)
         sate.current = me;
         return;
      // spurious context switch
      pthreaed_cond_wait (me->cond, csm)

 wait2 (me, m) :
   unlock (csm)
   ret = lock (m)
   lock (csm)
   ASSERT (current.tid == FREEMODE)
   if (state.current != me)
      LOG (THCTXSW, me)
   state.current = me
   return ret

 yield (me) :
   ASSERT (current.count <= 0)

   if (current.tid == FREEMODE)
      unlock (csm)
      return

   current = next context switch from replay
   if (current.tid == FREEMODE)
      cond_signal to all threads alive
   else
      cond_signal the thread (current.tid)
   unlock (csm)

 pthread_mutex_lock (m) :
   if (current.count >= 1)
      ASSERT (current.tid == me)
      lock (m)
   elseif (current.count == 0)
      ASSERT (current.tid == me)
      yield (me)
      wait (me, m)
      if (current.tid != FREEMODE)
         lock (m)
   else
      ASSERT (current.tid == FREEMODE)
      yield (me)
      lock (m)
      wait (me)
   LOG (MTXLOCK, m)
   current.count--;
   
 pthread_mutex_unlock (m) :
   unlock (m)
   LOG (MTXUNLK, m)
   current.count--

