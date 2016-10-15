
Design notes

Instrumentation
===============

- events
- structure rt
- pointers instrumented in the module, pointing to the host's heap:
  static struct rt * const rt; // stored in the Executor object, in the host
  static const uint64_t memstart;
  static const uint64_t memend;
  static const uint64_t evend;

- format of the event stream; at least 1 available
- _rt_storexx instrumented before the store; will stop execution if OOM fault
  and record one event


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
  So main must pthread_wait() for any other thread.

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
- when the application exit(3)'s, should it call pthread_cancel for every thread
  still alive?
- Can we return to the host from a thread different than main?
- How can we reclaim the stack from the same thread ??



  
continue here:
x how to insert main as the first thread
- how to prevent pthreads to call exit on the last thread
- implement pthread_exit


