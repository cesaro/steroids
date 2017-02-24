
#ifndef _RT_THREAD_SCHED_H_
#define _RT_THREAD_SCHED_H_

#include <pthread.h>

// Thread Control Block
struct rt_tcb
{
   /// flags, see below
   struct {
      unsigned alive : 1;    /// true iff the thread has been created and did not exit
      unsigned detached : 1; /// not used?
   } flags;

   /// scheduling state
   enum {
      SCHED_RUNNABLE,      /// thread can now run
      SCHED_WAIT_MUTEX,    /// thread is waiting for a mutex to become available
      SCHED_WAIT_JOIN,     /// thread is waiting for another thread to exit
      SCHED_WAIT_SS        /// thread is sleepset blocked
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


// the replay count can be "too short" only on the last context switch, on the
// previous context switches the number of events to replay needs to be exactly
// what the code consumes
#define REPLAY_CONSUME_ONE() \
   ASSERT (rt->replay.current == rt->replay.tab + rt->replay.size - 2 || \
         *rt->replay.current > 0 || *rt->replay.current == -1); \
   if (*rt->replay.current > 0) *rt->replay.current -= 1;

#endif
