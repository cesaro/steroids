
Tasks
-----

- Currently the unit tests do nothing. They contain some old, unmodified code.
- Improve the stid testing tool to embded a copy of the rt.bc, or to find it
  somewhere in the file system
- Write .test.sh files for the regression tests in tests/regression/*
- When the program does not contain all functions it calls and the remaining
  ones cannot be found in in the host process (or loaded libraries) the call to
  ee->finalizeObject() fails and I cannot catch the error
- when preserving data segments, we should only preserve initialized data
  segments, such as .data and .rodata, not .bss !!
- for those pthread functions that we do not handle, issue a warning in the
  stream

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

Done
----

- how to insert main as the first thread
- how to prevent pthreads to call exit on the last thread
- improve support for arbitrary types on load/store
- show we generate events RD|WR for 1 bit ?

Questions
---------

- when the application exit(3)'s, should it call pthread_cancel for every
  thread still alive?
- Can we return to the host from a thread different than main?
- How can we reclaim the stack from the same thread ??
- with the current implementation, how to detect a deadlock in the runtime?
- we make copy of the pointers stdio, stderr, and stdin inside the guest and
  initialize it the beginning, but this won't work if someones wants to modify
  the value of these pointers. How to deal in general with the problem of
  having variables in the libc that are accessible in the program?

