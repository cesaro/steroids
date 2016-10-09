
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
