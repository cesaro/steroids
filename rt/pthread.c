
#include <pthread.h> // in /usr/include
#include "pthread.h" // in .
#include "thread-sched.h"


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
   struct rt_tcb *me = __state.current;

   _printf ("stid: rt: threading: t%d: "
         "pthread_create (tid=%p, attr=%p, start=%p, arg=%p); replay %d\n",
         TID (me), tid, attr, start, arg, *rt->replay.current);

   // get a TCB
   if (__state.next >= RT_MAX_THREADS) return ENOMEM;
   t = __state.tcbs + __state.next++;

   // initialize the TCB
   t->flags.alive = 1;
   t->flags.needsjoin = 0;
   t->flags.detached = 0;
   t->state = SCHED_RUNNABLE;
   // wait_join and wait_mutex do not need to be initialized
   t->start = start;
   t->arg = arg;
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
   ret = __rt_thread_stack_alloc (t, (pthread_attr_t *) attr);
   if (ret) goto err_stack_alloc;

   // start the thread
   ret = pthread_create (&t->tid, attr, __rt_thread_start, t);
   if (ret) goto err_create;

   // if necessary, destroy the new attributes, ignore errors
   if (need_destroy) pthread_attr_destroy (&attr2);

   // the thread is now alive
   __state.num_ths_alive++;

   // log the event
   _printf ("stid: rt: threading: t%d: new thread t%d, stack %p size %zu%s\n",
         TID (me),
         TID (t),
         t->stackaddr,
         UNITS_SIZE (t->stacksize),
         UNITS_UNIT (t->stacksize));
   TRACE3 (RT_THCREAT, TID (t));
   rt->trace.num_blue[TID(me)]++;

   // consume one event in the replay sequence
   REPLAY_CONSUME_ONE ();

   // we use the pthread_t variable to store a pointer to our tcb
   ASSERT (sizeof (pthread_t) >= sizeof (struct rt_tcb*));
   * ((struct rt_tcb **) tid) = t;
   return 0;

err_create:
   __rt_thread_stack_free (t); // deallocate the stack
err_stack_alloc:
   if (need_destroy) pthread_attr_destroy (&attr2); // ignore possible error
err_attr_init:
   pthread_cond_destroy (&t->cond); // ignore possible error
err_cond:
   __state.next--; // release the tcb
   return ret;
}

int   _rt_pthread_detach(pthread_t t)
{
   // main needs to JOIN for them, we do not support this for the time begin
   (void) t;
   PRINT ("warning: pthread_detach called but we do not have support for it");
   return EINVAL;
}

//int   _rt_pthread_equal(pthread_t t1, pthread_t t2) { return -1; }
int   _rt_pthread_join(pthread_t t, void **retval)
{
   int ret;
   struct rt_tcb *other = (struct rt_tcb *) t;
   struct rt_tcb *me = __state.current;

   _printf ("stid: rt: threading: t%d: "
         "pthread_join (other=t%d, retval=%p); replay %d\n",
         TID (me), TID(other), retval, *rt->replay.current);

   // validate arguments
   if (other < __state.tcbs || other > __state.tcbs + RT_MAX_THREADS)
      return EINVAL;

   // declare my new state and execute other threads if necessary
   me->state = SCHED_WAIT_JOIN;
   me->wait_join = other;
   __rt_thread_protocol_yield ();

   // when we are scheduled again we can join without blocking
   ASSERT (other->flags.alive == 0);
   ASSERT (other->flags.needsjoin == 1);
   ret = pthread_join (other->tid, 0);
   if (ret) return ret;

   // mark that we already joined for this thread
   other->flags.needsjoin = 0;

   // log event, and consume one from the replay sequence
   TRACE3 (RT_THJOIN, TID (other));
   REPLAY_CONSUME_ONE ();
   rt->trace.num_blue[TID(me)]++;

   // write the retval if user interested
   if (retval) *retval = other->retval;
   return 0;
}

void  _rt_pthread_exit(void *retval)
{
   struct rt_tcb *me = __state.current;
   int ret;
   unsigned i;

   _printf ("stid: rt: threading: t%d: "
         "pthread_exit (retval=%p); replay %d, alive %d\n",
         TID (me), retval, *rt->replay.current, __state.num_ths_alive);

   // if we are the main thread, either we exit with status 0 (if no other
   // thread is alive) or we wait for the other threads to finish, join for the
   // NPTL threads, and then exit with status 0
   if (TID (me) == 0)
   {
      if (__state.num_ths_alive >= 2)
      {
         // wait for all threads to finish
         me->state = SCHED_WAIT_ALLEXIT;
         __rt_thread_protocol_yield ();
         ASSERT (__state.num_ths_alive == 1); // only me!

         // NPTL join (releasea resources, otherwise I get segfaults...)
         for (i = 0; i < __state.next; i++)
         {
            if (__state.tcbs[i].flags.needsjoin)
            {
               _printf ("stid: rt: threading: t%d: joining for t%d\n", TID (me), i);
               if (pthread_join (__state.tcbs[i].tid, 0))
                  PRINT ("t%d: exit: errors while joinng for t%d; ignoring", TID(me), i);
            }
         }
      }
      _rt_exit (0);
   }

   // otherwise, log the _THEXIT event and decrement number of threads alive
   TRACE0 (RT_THEXIT);
   rt->trace.num_blue[TID(me)]++;
   __state.num_ths_alive--;
   me->retval = retval;
   me->flags.alive = 0;
   me->flags.needsjoin = 1;

   // consume one event in the replay sequence
   REPLAY_CONSUME_ONE ();
   ASSERT (*rt->replay.current == -1 || *rt->replay.current == 0);

   // destroy my conditional variable
   ret = pthread_cond_destroy (&me->cond); // ignore error
   if (ret)
   {
      PRINT ("t%d: cond var: errors while destroying: %s; ignoring",
            TID(me), strerror (ret));
   }

   // FIXME - free the stack!!!

   // exit protocol: we release the cs mutex; yield() will detect that this
   // thread is not alive and will come back without locking again the cs mutex
   __rt_thread_protocol_yield ();

   // terminate this thread
   pthread_exit (0);
}

pthread_t _rt_pthread_self (void)
{
   return __state.current->tid;
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
   if (type != PTHREAD_MUTEX_NORMAL)
   {
      PRINT ("pthread_mutexattr_settype: "
            "setting type != PTHREAD_MUTEX_NORMAL is unsupported");
      return EINVAL;
   }
   return 0;
}
int   _rt_pthread_mutexattr_setrobust(pthread_mutexattr_t *a, int rob)
{
   // We only support robust-mutexes (the default value)
   (void) a;
   if (rob != PTHREAD_MUTEX_STALLED)
   {
      PRINT ("pthread_mutexattr_setrobust: "
            "setting rob != PTHREAD_MUTEX_STALLED is unsupported");
      return EINVAL;
   }
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
   PRINT ("pthread_mutex_consistent: call to unsuported primitive");
   return EINVAL;
}

int   _rt_pthread_mutex_lock(pthread_mutex_t *m)
{
   _printf ("stid: rt: threading: t%d: "
         "pthread_mutex_lock (m=%p); replay %d\n",
         TID (__state.current), m, *rt->replay.current);

   struct rt_tcb *me = __state.current;
   int ret;

   // update the scheduling state and reschedule threads
   me->state = SCHED_WAIT_MUTEX;
   me->wait_mutex = m;
   __rt_thread_protocol_yield ();

   // locking now is non-blocking, return possible errors
   ret = pthread_mutex_lock (m);
   if (ret) return ret;
   
   // log the event (only after actually locking m!), consume 1 from replay
   TRACE1 (RT_MTXLOCK, m);
   REPLAY_CONSUME_ONE ();
   rt->trace.num_blue[TID(me)]++;

   return 0;
}

int   _rt_pthread_mutex_unlock(pthread_mutex_t *m)
{
   _printf ("stid: rt: threading: t%d: "
         "pthread_mutex_unlock (m=%p); replay %d\n",
         TID (__state.current), m, *rt->replay.current);

   struct rt_tcb *me = __state.current;
   int ret;

   // unlock the mutex
   ret = pthread_mutex_unlock (m);
   if (ret) return ret;

   // trace the evend
   TRACE1 (RT_MTXUNLK, m);
   REPLAY_CONSUME_ONE ();
   rt->trace.num_blue[TID(me)]++;

   return 0;
}

int   _rt_pthread_mutex_timedlock(pthread_mutex_t *restrict m,
      const struct timespec *restrict tm)
{
   (void) m;
   (void) tm;
   // FIXME -- issue a warning in the stream
   PRINT ("pthread_mutex_timedlock: call to unsuported primitive");
   return EINVAL;
}

int   _rt_pthread_mutex_trylock(pthread_mutex_t * m)
{
   (void) m;
   // FIXME -- issue a warning in the stream
   PRINT ("pthread_mutex_trylock: call to unsuported primitive");
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
int   _rt_pthread_cond_broadcast(pthread_cond_t *cond)
{
   (void) cond;
   PRINT ("pthread_cond_broadcast: call to unsuported primitive");
   __rt_panic ();
   return 0;
}
int   _rt_pthread_cond_destroy(pthread_cond_t *cond)
{
   (void) cond;
   PRINT ("pthread_cond_destroy: call to unsuported primitive");
   __rt_panic ();
   return 0;
}
int   _rt_pthread_cond_init(pthread_cond_t *restrict cond,
      const pthread_condattr_t *restrict attr)
{
   (void) cond;
   (void) attr;
   PRINT ("pthread_cond_init: call to unsuported primitive");
   __rt_panic ();
   return 0;
}
int   _rt_pthread_cond_signal(pthread_cond_t *cond)
{
   (void) cond;
   PRINT ("pthread_cond_signal: call to unsuported primitive");
   __rt_panic ();
   return 0;
}
int   _rt_pthread_cond_timedwait(pthread_cond_t *restrict cond,
            pthread_mutex_t *restrict attr, const struct timespec *restrict tm)
{
   (void) cond;
   (void) attr;
   (void) tm;
   PRINT ("pthread_cond_timedwait: call to unsuported primitive");
   __rt_panic ();
   return 0;
}
int   _rt_pthread_cond_wait(pthread_cond_t *restrict cond,
         pthread_mutex_t *restrict m)
{
   (void) cond;
   (void) m;
   PRINT ("pthread_cond_wait: call to unsuported primitive");
   __rt_panic ();
   return 0;
}

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



int __rt_thread_stack_alloc (struct rt_tcb *t, pthread_attr_t *attr)
{
   int ret;

   // get stack paramters from a
   ret = pthread_attr_getstack (attr, &t->stackaddr, &t->stacksize);
   if (ret) goto err_getstack;

   // user already set up the stack
   if (t->stackaddr != 0) return 0;
   if (t->stacksize == 0) t->stacksize = rt->default_thread_stack_size;

   // FIXME - malloc
   t->stackaddr = _rt_malloc_uninitialized (t->stacksize);
   if (! t->stackaddr)
   {
      ret = ENOMEM;
      goto err_malloc;
   }

   // tell NPTL about this stack configuration
   ret = pthread_attr_setstack (attr, t->stackaddr, t->stacksize);
   if (ret) goto err_setstack;
   return 0;

err_setstack:
   __rt_thread_stack_free (t->stackaddr);
err_malloc:
err_getstack:
   return ret;
}

void  __rt_thread_stack_free (struct rt_tcb *t)
{
   _rt_free (t->stackaddr);
}

