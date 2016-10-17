
#include <pthread.h> // in /usr/include
#include "pthread.h" // in .


struct {
   // global mutex, only 1 thread executes at a time
   pthread_mutex_t m;
   // the state of every thread
   struct rt_tcb tcbs[RT_MAX_THREADS];
   // the index of the next available thread TCB
   int next;
   // only one thread executes at a time, pointer stored here
   struct rt_tcb *current;
   // number of threads currently alive
   int num_ths_alive;
} __rt_thst;

#define TID(t) ((int) ((t) - __rt_thst.tcbs))


// default implementation is good for us:
//int   _rt_pthread_attr_init(pthread_attr_t *a) { return -1; }
//int   _rt_pthread_attr_destroy(pthread_attr_t *a) { return -1; }
//int   _rt_ pthread_attr_getdetachstate(const pthread_attr_t *a, int *d) { return -1; }

int   _rt_pthread_attr_setdetachstate(pthread_attr_t *a, int s)
{
   // we only support creating threads in JOINABLE mode
   (void) a;
   if (s != PTHREAD_CREATE_JOINABLE) return EINVAL;
   return 0;
}

int   _rt_pthread_attr_getguardsize(const pthread_attr_t *a, size_t *s)
{
   // we do not support a guardarea at the end of the stack
   (void) a;
   (void) s;
   return EINVAL;
}
int   _rt_pthread_attr_setguardsize(pthread_attr_t *a, size_t s)
{
   (void) a;
   (void) s;
   return EINVAL;
}

// default implementations good:
//int   _rt_pthread_attr_getinheritsched(const pthread_attr_t *a, int *i) { return -1; }
//int   _rt_pthread_attr_getschedparam(const pthread_attr_t *a, struct sched_param *p) { return -1; }
//int   _rt_pthread_attr_getschedpolicy(const pthread_attr_t *a, int *p) { return -1; }
//int   _rt_pthread_attr_getscope(const pthread_attr_t *a, int *s) { return -1; }
//int   _rt_pthread_attr_getstack(const pthread_attr_t *restrict a, void **restrict addr, size_t *restrict size) { return -1; }
//int   _rt_pthread_attr_setstack(pthread_attr_t *a, void *addr, size_t size) { return -1; }
//int   _rt_pthread_attr_getstacksize(const pthread_attr_t *a, size_t *p) { return -1; }
//int   _rt_pthread_attr_setstacksize(pthread_attr_t *a, size_t s) { return -1; }
//int   _rt_pthread_attr_setinheritsched(pthread_attr_t *a, int i) { return -1; }
//int   _rt_pthread_attr_setschedparam(pthread_attr_t *a, const struct sched_param *p) { return -1; }
//int   _rt_pthread_attr_setschedpolicy(pthread_attr_t *a, int p) { return -1; }
//int   _rt_pthread_attr_setscope(pthread_attr_t *a, int s) { return -1; }

int   _rt_pthread_create(pthread_t *tid,
      const pthread_attr_t *attr, void *(*start)(void *), void *arg)
{
   int ret;
   struct rt_tcb *t;
   pthread_attr_t attr2;
   int need_destroy = 0;

   // get a TCB
   if (__rt_thst.next >= RT_MAX_THREADS) return ENOMEM;
   t = __rt_thst.tcbs + __rt_thst.next++;

   // initialize the TCB
   t->flags.alive = 1;
   t->flags.detached = 0;
   t->start = start;
   t->arg = arg;
   t->ownedmut_size = 0;
   ret = pthread_cond_init (&t->cond, 0);
   if (ret) goto err_cond; // we return the same error

   // if we got no attributes, create default ones
   if (! attr)
   {
      ret = pthread_attr_init (&attr2);
      if (ret) goto err_attr_init;
      attr = &attr2;
      need_destroy = 1;
   }

   // get space for the stack and tell NPTL about it, setting the attr
   // structure. FIXME - we are not supposed to do this!!
   ret = _rt_thread_stack_alloc (t, (pthread_attr_t *) attr);
   if (ret) goto err_stack_alloc;

   // start the thread
   ret = pthread_create (tid, attr, _rt_thread_start, t);
   if (ret) goto err_create;
   t->tid = *tid;

   // if necessary, destroy the new attributes, ignore errors
   if (need_destroy) pthread_attr_destroy (&attr2);

   // the thread is now alive
   __rt_thst.num_ths_alive++;

   // log the event
   printf ("stid: rt: threading: created new thread t%d, stack %p size %zu%s\n",
         TID (t),
         t->stackaddr,
         UNITS_SIZE (t->stacksize),
         UNITS_UNIT (t->stacksize));
   TRACE3 (RT_THCREAT, TID (t));
   return 0;

err_create:
   _rt_thread_stack_free (t); // deallocate the stack
err_stack_alloc:
   if (need_destroy) pthread_attr_destroy (&attr2); // ignore possible error
err_attr_init:
   pthread_cond_destroy (&t->cond); // ignore possible error
err_cond:
   __rt_thst.next--; // release the tcb
   return ret;
}

int   _rt_pthread_detach(pthread_t t)
{
   // main needs to JOIN for them, we do not support this for the time begin
   (void) t;
   return EINVAL;
}

//int   _rt_pthread_equal(pthread_t t1, pthread_t t2) { return -1; }
int   _rt_pthread_join(pthread_t t, void **retval)
{
   int ret;
   struct rt_tcb *other = 0;
   struct rt_tcb *me = __rt_thst.current;

   _rt_thread_protocol_yield (me);
   ret = pthread_join (t, (void *) &other);
   _rt_thread_protocol_wait (me);

   // possibly return with error, log event, and write the retval if user interested
   if (ret) return ret;
   TRACE3 (RT_THJOIN, TID (other));
   if (retval) *retval = other->retval;
   return 0;
}

void  _rt_pthread_exit(void *retval)
{
   struct rt_tcb *me = __rt_thst.current;
   printf ("stid: rt: threading: t%d: exiting!\n", TID (me));

   // if we are the main thread, we exit with status 0
   // FIXME - this is strictly a violation of what POSIX says, we should wait
   // for the others, but the current implementation does not support that
   if (TID (me) == 0) _rt_exit (0);

   // otherwise, log the _THEXIT event and decrement number of threads alive
   TRACE0 (RT_THEXIT);
   __rt_thst.num_ths_alive--;
   me->retval = retval;
   me->flags.alive = 0;

   // FIXME - free the stack!!!

   // exit protocol: we release the cs mutex
   _rt_thread_protocol_yield (me);

   // terminate this thread and return retval
   pthread_exit (me);
}

pthread_t _rt_pthread_self (void)
{
   return __rt_thst.current->tid;
}

// thread cancellation
int   _rt_pthread_cancel(pthread_t t)
{
   (void) t;
   PRINT ("pthread_cancel called and we do not have support for it");
   return EINVAL;
}
int   _rt_pthread_setcancelstate(int state, int *oldstate)
{
   (void) state;
   (void) oldstate;
   // FIXME -- issue a warning in the stream
   PRINT ("pthread_setcancelstate called and we do not have support for it");
   return EINVAL;
}
int   _rt_pthread_setcanceltype(int type, int *oldtype)
{
   (void) type;
   (void) oldtype;
   // FIXME -- issue a warning in the stream
   PRINT ("pthread_setcanceltype called and we do not have support for it");
   return EINVAL;
}
void  _rt_pthread_testcancel(void)
{
   // FIXME -- issue a warning in the stream
   PRINT ("pthread_test_cancel called and we do not have support for it");
}
// FIXME - guess how to support or not this
#if 0
void  _rt_pthread_cleanup_push(void (*routine)(void *arg), void *arg)
{
   (void) routine;
   (void) arg;
   // FIXME -- issue a warning in the stream
   PRINT ("pthread_cleanup_push called and we do not have support for it");
   pthread_cleanup_push (routine, arg);
}
void  _rt_pthread_cleanup_pop(int ex)
{
   (void) ex;
   PRINT ("pthread_cleanup_pop called and we do not have support for it");
   pthread_cleanup_pop (ex);
}
#endif

// attributes for mutexes
//int   _rt_pthread_mutexattr_init(pthread_mutexattr_t *);
//int   _rt_pthread_mutexattr_destroy(pthread_mutexattr_t *);
//int   _rt_pthread_mutexattr_getprioceiling( const pthread_mutexattr_t *restrict, int *restrict);
//int   _rt_pthread_mutexattr_getprotocol(const pthread_mutexattr_t *restrict, int *restrict);
//int   _rt_pthread_mutexattr_getpshared(const pthread_mutexattr_t *restrict, int *restrict);
//int   _rt_pthread_mutexattr_getrobust(const pthread_mutexattr_t *restrict, int *restrict);
//int   _rt_pthread_mutexattr_gettype(const pthread_mutexattr_t *restrict, int *restrict);
//int   _rt_pthread_mutexattr_setprioceiling(pthread_mutexattr_t *, int);
//int   _rt_pthread_mutexattr_setprotocol(pthread_mutexattr_t *, int);
//int   _rt_pthread_mutexattr_setpshared(pthread_mutexattr_t * a, int s)
int   _rt_pthread_mutexattr_settype(pthread_mutexattr_t *a, int type)
{
   (void) a;
   if (type != PTHREAD_MUTEX_NORMAL) return EINVAL;
   return 0;
}
int   _rt_pthread_mutexattr_setrobust(pthread_mutexattr_t *a, int rob)
{
   // We only support robust-mutexes (the default value)
   (void) a;
   if (rob != PTHREAD_MUTEX_STALLED) return EINVAL;
   return 0;
}

// mutexes
//int   _rt_pthread_mutex_init(pthread_mutex_t *restrict, const pthread_mutexattr_t *restrict);
//int   _rt_pthread_mutex_destroy(pthread_mutex_t *);
//int   _rt_pthread_mutex_getprioceiling(const pthread_mutex_t *restrict, int *restrict);
//int   _rt_pthread_mutex_setprioceiling(pthread_mutex_t *restrict, int, int *restrict);
int   _rt_pthread_mutex_consistent(pthread_mutex_t *m)
{
   (void) m;
   return EINVAL;
}
int   _rt_pthread_mutex_lock(pthread_mutex_t *m)
{
   struct rt_tcb *me = __rt_thst.current;
   int ret;

   _rt_thread_protocol_yield (me);
   ret = pthread_mutex_lock (m);
   _rt_thread_protocol_wait (me);
   TRACE1 (RT_MTXLOCK, m);
   return ret;
}

int   _rt_pthread_mutex_unlock(pthread_mutex_t *m)
{
   int ret;
   ret = pthread_mutex_unlock (m);
   TRACE1 (RT_MTXUNLK, m);
   return ret;
}

int   _rt_pthread_mutex_timedlock(pthread_mutex_t *restrict m,
      const struct timespec *restrict tm)
{
   (void) m;
   (void) tm;
   // FIXME -- issue a warning in the stream
   PRINT ("pthread_mutex_timedlock called and we do not have support for it");
   return EINVAL;
}

int   _rt_pthread_mutex_trylock(pthread_mutex_t * m)
{
   (void) m;
   // FIXME -- issue a warning in the stream
   PRINT ("pthread_mutex_trylock called and we do not have support for it");
   return EINVAL;
}

// attributes for conditional variables
int   _rt_pthread_condattr_destroy(pthread_condattr_t *);
int   _rt_pthread_condattr_getclock(const pthread_condattr_t *restrict, clockid_t *restrict);
int   _rt_pthread_condattr_getpshared(const pthread_condattr_t *restrict, int *restrict);
int   _rt_pthread_condattr_init(pthread_condattr_t *);
int   _rt_pthread_condattr_setclock(pthread_condattr_t *, clockid_t);
int   _rt_pthread_condattr_setpshared(pthread_condattr_t *, int);

// conditional variables
int   _rt_pthread_cond_broadcast(pthread_cond_t *);
int   _rt_pthread_cond_destroy(pthread_cond_t *);
int   _rt_pthread_cond_init(pthread_cond_t *restrict, const pthread_condattr_t *restrict);
int   _rt_pthread_cond_signal(pthread_cond_t *);
int   _rt_pthread_cond_timedwait(pthread_cond_t *restrict, pthread_mutex_t *restrict, const struct timespec *restrict);
int   _rt_pthread_cond_wait(pthread_cond_t *restrict, pthread_mutex_t *restrict);

// attributes for read-write locks
int   _rt_pthread_rwlockattr_destroy(pthread_rwlockattr_t *);
int   _rt_pthread_rwlockattr_getpshared( const pthread_rwlockattr_t *restrict, int *restrict);
int   _rt_pthread_rwlockattr_init(pthread_rwlockattr_t *);
int   _rt_pthread_rwlockattr_setpshared(pthread_rwlockattr_t *, int);

// read-write locks
int   _rt_pthread_rwlock_destroy(pthread_rwlock_t *);
int   _rt_pthread_rwlock_init(pthread_rwlock_t *restrict, const pthread_rwlockattr_t *restrict);
int   _rt_pthread_rwlock_rdlock(pthread_rwlock_t *);
int   _rt_pthread_rwlock_timedrdlock(pthread_rwlock_t *restrict, const struct timespec *restrict);
int   _rt_pthread_rwlock_timedwrlock(pthread_rwlock_t *restrict, const struct timespec *restrict);
int   _rt_pthread_rwlock_tryrdlock(pthread_rwlock_t *);
int   _rt_pthread_rwlock_trywrlock(pthread_rwlock_t *);
int   _rt_pthread_rwlock_unlock(pthread_rwlock_t *);
int   _rt_pthread_rwlock_wrlock(pthread_rwlock_t *);

// spin locks
int   _rt_pthread_spin_destroy(pthread_spinlock_t *);
int   _rt_pthread_spin_init(pthread_spinlock_t *, int);
int   _rt_pthread_spin_lock(pthread_spinlock_t *);
int   _rt_pthread_spin_trylock(pthread_spinlock_t *);
int   _rt_pthread_spin_unlock(pthread_spinlock_t *);

// barriers
int   _rt_pthread_barrier_destroy(pthread_barrier_t *);
int   _rt_pthread_barrier_init(pthread_barrier_t *restrict, const pthread_barrierattr_t *restrict, unsigned);
int   _rt_pthread_barrier_wait(pthread_barrier_t *);
int   _rt_pthread_barrierattr_destroy(pthread_barrierattr_t *);
int   _rt_pthread_barrierattr_getpshared( const pthread_barrierattr_t *restrict, int *restrict);
int   _rt_pthread_barrierattr_init(pthread_barrierattr_t *);
int   _rt_pthread_barrierattr_setpshared(pthread_barrierattr_t *, int);

// TLS
int   _rt_pthread_key_create(pthread_key_t *, void (*)(void*));
int   _rt_pthread_key_delete(pthread_key_t);
void *_rt_pthread_getspecific(pthread_key_t);
int   _rt_pthread_setspecific(pthread_key_t, const void *);

// parameters about thread scheduling
int   _rt_pthread_getconcurrency(void);
int   _rt_pthread_setconcurrency(int);
int   _rt_pthread_getschedparam(pthread_t, int *restrict, struct sched_param *restrict);
int   _rt_pthread_setschedparam(pthread_t, int, const struct sched_param *);
int   _rt_pthread_setschedprio(pthread_t, int);

// miscellanea
int   _rt_pthread_once(pthread_once_t *, void (*)(void));
int   _rt_pthread_getcpuclockid(pthread_t, clockid_t *);
int   _rt_pthread_atfork(void (*)(void), void (*)(void), void(*)(void));


void  _rt_thread_init (void)
{
   int ret;

   // whoever calls this function becomes the main thread (tid = 0)
   printf ("stid: rt: threading: initializing the multithreading library\n");
   __rt_thst.next = 1;
   __rt_thst.current = __rt_thst.tcbs;
   __rt_thst.num_ths_alive = 1;

   ret = pthread_mutex_init (&__rt_thst.m, 0);
   if (ret)
   {
      // FIXME -- issue a warning into the stream here
      printf ("stid: rt: threading: initializing cs mutex: error: %s\n",
            strerror (ret));
   }
   _rt_thread_protocol_wait (__rt_thst.current);
}

void  _rt_thread_term (void)
{
   int ret;

   printf ("stid: rt: threading: terminating the multithreading library\n");

   // only the main thread should be calling this, stop the model checker
   // otherwise
   if (TID (__rt_thst.current) != 0)
   {
      PRINT ("error: thread %d called exit() but this runtime "
            "only supports calls to exit() from the main thread",
            TID (__rt_thst.current));
      exit (1);
   }

   // and it should call it only when the main thread is the only one alive
   if  (__rt_thst.num_ths_alive > 1)
   {
      PRINT ("error: main thread called exit() but %d other threads are still alive;"
            " this is not currently supported by the runtime",
            __rt_thst.num_ths_alive - 1);
      exit (1);
   }

   // copy information to the trace structure
   rt->trace.num_ths = __rt_thst.next;
   //rt->trace.num_mutex = 0;

   // release the cs mutex (no other thread can acquire it)
   _rt_thread_protocol_yield (__rt_thst.current);


   // destroy the cs mutex
   ret = pthread_mutex_destroy (&__rt_thst.m);
   if (ret)
   {
      // FIXME -- issue a warning into the stream here
      printf ("stid: rt: threading: destroying cs mutex: error: %s\n",
            strerror (ret));
   }
}

void *_rt_thread_start (void *arg)
{
   struct rt_tcb *t = (struct rt_tcb *) arg;
   void *ret;

   printf ("stid: rt: threading: start: t%d: starting!\n", TID (t));

   // start protocol: we wait to get our context switch
   _rt_thread_protocol_wait (t);

   // run the function provided, with the good argument
   ret = t->start (t->arg);

   // exit the thread through one unique place in the code
   _rt_pthread_exit (ret);

   return ret; // unreachable
}

void _rt_thread_protocol_wait (struct rt_tcb *t)
{
   int ret;

   // lock on the global mutex
   ret = pthread_mutex_lock (&__rt_thst.m);
   if (ret != 0) goto err_panic;

   while (1)
   {
      printf ("stid: rt: threading: proto: t%d: acquired cs lock\n", TID (t));
      if (__rt_thst.current == t)
      {
         printf ("stid: rt: threading: ctxsw to same thread, skipping THCTXSW\n");
      }
      else 
      {
         TRACE3 (RT_THCTXSW, TID (t));
      }
      __rt_thst.current = t;
      return;
      // FIXME - if first event or replay == CS or ...; then return
      // else pthread_cond_wait (&t->cond, &__rt_thst.m);

      ASSERT (0);
   }

err_panic :
   PRINT ("error: t%d: acquiring internal mutex: %s", TID (t), strerror (ret));
   _rt_cend (255);
}

void _rt_thread_protocol_yield (struct rt_tcb *t)
{
   int ret;

   // FIXME - if something about the events, then signal

   // unlock on the global mutex
   ret = pthread_mutex_unlock (&__rt_thst.m);
   if (ret != 0) goto err_panic;

   printf ("stid: rt: threading: proto: t%d: released cs lock\n", TID (t));
   return;

err_panic :
   PRINT ("error: t%d: releasing internal mutex: %s", TID (t), strerror (ret));
   _rt_cend (255);
}

int _rt_thread_stack_alloc (struct rt_tcb *t, pthread_attr_t *attr)
{
   int ret;

   // get stack paramters from a
   ret = pthread_attr_getstack (attr, &t->stackaddr, &t->stacksize);
   if (ret) goto err_getstack;

   // user already set up the stack
   if (t->stackaddr != 0) return 0;
   if (t->stacksize == 0) t->stacksize = RT_DEFAULT_STACK_SIZE;

   // FIXME - malloc
   t->stackaddr = _rt_malloc (t->stacksize);
   if (! t->stackaddr) goto err_malloc;

   // tell NPTL about this stack configuration
   ret = pthread_attr_setstack (attr, t->stackaddr, t->stacksize);
   if (ret) goto err_setstack;
   return 0;

err_setstack:
   _rt_thread_stack_free (t->stackaddr);
err_malloc:
err_getstack:
   return ret;
}

void  _rt_thread_stack_free (struct rt_tcb *t)
{
   _rt_free (t->stackaddr);
}

