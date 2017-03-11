
#ifndef _RT_THREAD_SCHED_H_
#define _RT_THREAD_SCHED_H_

#include <pthread.h>

// Thread Control Block
struct rt_tcb
{
   /// flags, see below
   struct {
      unsigned alive : 1;     /// true iff the thread has been created and did not exit
      unsigned needsjoin : 1; /// true iff the thread has been alive but no thread joined for him so far
      unsigned detached : 1;  /// not used?
   } flags;

   /// scheduling state
   enum {
      SCHED_RUNNABLE,      /// thread can now run
      SCHED_WAIT_MUTEX,    /// thread is waiting for a mutex to become available
      SCHED_WAIT_JOIN,     /// thread is waiting for another thread to exit
      SCHED_WAIT_SS,       /// thread is sleepset blocked
      SCHED_WAIT_ALLEXIT   /// thread is waiting for all other threads to finish (only main thread)
   } state;

   /// when in SCHED_WAIT_JOIN, the thread is waiting for this thread
   struct rt_tcb *wait_join;
   /// when in SCHED_WAIT_MUTEX, the thread is waiting for this mutex
   pthread_mutex_t *wait_mutex;
   /// handle for the NPTL library
   pthread_t tid;
   /// conditional variable, for the scheduling protocol
   pthread_cond_t cond;

   // the arguments of pthread_create and return value of the thread
   void *(*start)(void *);
   void *arg;
   void *retval;

   // location of the stack
   void  *stackaddr;
   size_t stacksize;
};

void  __rt_thread_init (void);
void  __rt_thread_term (void);
void *__rt_thread_start (void *arg);
void  __rt_thread_protocol_wait_first ();
void  __rt_thread_protocol_wait (struct rt_tcb *t);
void  __rt_thread_protocol_yield ();

int            __rt_thread_sched_update (struct rt_tcb *t);
struct rt_tcb* __rt_thread_sched_find_any ();
struct rt_tcb* __rt_thread_sched_find_next ();

void __rt_thread_sleepset_init ();
void __rt_thread_sleepset_awake (pthread_mutex_t *m);


// the replay count, as generated from DPU or other tools will often be "too
// short" meaning that the event on which the count becomes zero doesn't need to
// be a context-switch point (lock or join). For instance, conside the following
// configuration, and assume that you need to replay the event e, which makes
// part of some alternative:
//
// t1      t2
// ------- --------
// start
// |
// lock x
// |
// unlock x
// |    \
// end   \
//        lock x
//        |
//        unlock x (e)
//
// To replay that DPU will generate the sequence [t1 3; t2 2], which asks to stop t1
// just after the "unlock x". The event "end" is not a context-switch point, so
// steroids is unable to context switch there. Instead we continue executing
// until the next context switch point happens, and do the context switch there.
// The additional events that we introduce are necessary local (??).
#define REPLAY_CONSUME_ONE() \
   if (*rt->replay.current > 0) *rt->replay.current -= 1

#endif
