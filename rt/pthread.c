
// continue here:
// - how to prevent pthreads to call exit on the last thread
// - how to insert main as the first thread
// - implement pthread_exit


#include <pthread.h> // in /usr/include
#include "pthread.h" // in .


struct {
   // global mutex, only 1 thread executes at a time
   pthread_mutex_t m;
   // the state of every thread
   struct rt_thread handles[RT_MAX_THREADS];
   // the index of the next available thread handle
   int next;
   // only one thread executes at a time, pointer stored here
   struct rt_thread *current;
} __rt_thst;

#define TID(t) ((int) ((t) - __rt_thst.handles))


// thread attributes
int   _rt_pthread_attr_init(pthread_attr_t *a)
{
   return -1;
}
int   _rt_pthread_attr_destroy(pthread_attr_t *a)
{
   return -1;
}
int   _rt_pthread_attr_getdetachstate(const pthread_attr_t *a, int *d)
{
   return -1;
}
int   _rt_pthread_attr_getguardsize(const pthread_attr_t *a, size_t *s)
{
   return -1;
}
int   _rt_pthread_attr_getinheritsched(const pthread_attr_t *a, int *i)
{
   return -1;
}
int   _rt_pthread_attr_getschedparam(const pthread_attr_t *a, struct sched_param *p)
{
   return -1;
}
int   _rt_pthread_attr_getschedpolicy(const pthread_attr_t *a, int *p)
{
   return -1;
}
int   _rt_pthread_attr_getscope(const pthread_attr_t *a, int *s)
{
   return -1;
}
int   _rt_pthread_attr_getstackaddr(const pthread_attr_t *a, void **p)
{
   return -1;
}
int   _rt_pthread_attr_getstacksize(const pthread_attr_t *a, size_t *p)
{
   return -1;
}
int   _rt_pthread_attr_setdetachstate(pthread_attr_t *a, int s)
{
   return -1;
}
int   _rt_pthread_attr_setguardsize(pthread_attr_t *a, size_t s)
{
   return -1;
}
int   _rt_pthread_attr_setinheritsched(pthread_attr_t *a, int i)
{
   return -1;
}
int   _rt_pthread_attr_setschedparam(pthread_attr_t *a, const struct sched_param *p)
{
   return -1;
}
int   _rt_pthread_attr_setschedpolicy(pthread_attr_t *a, int p)
{
   return -1;
}
int   _rt_pthread_attr_setscope(pthread_attr_t *a, int s)
{
   return -1;
}
int   _rt_pthread_attr_setstackaddr(pthread_attr_t *a, void *p)
{
   return -1;
}
int   _rt_pthread_attr_setstacksize(pthread_attr_t *a, size_t s)
{
   return -1;
}

// thread creation / joining / exiting
int   _rt_pthread_create(pthread_t *tid,
      const pthread_attr_t *attr, void *(*start)(void *), void *arg)
{
   int i, ret;
   struct rt_thread *t;
   ASSERT (sizeof (pthread_t) >= sizeof (unsigned));

   // get a handle
   if (__rt_thst.next >= RT_MAX_THREADS) return ENOMEM;
   t = __rt_thst.handles + __rt_thst.next++;

   // initialize the handle
   t->flags = 0;
   t->start = start;
   t->arg = arg;
   ret = pthread_cond_init (&t->cond, 0);
   if (ret) goto err_cond; // we return the same error

   // start the thread, writing the tid in the handle
   ret = pthread_create (&t->tid, attr, _rt_thread_start, t);
   if (ret) goto err_create;

   // success
   printf ("stid: rt: threading: created new thread t%d\n", TID(t));
   // @TODO: missing increment eventrace num_ths
   TRACE3 (_THCREAT, t - __rt_thst.handles);
   return 0;

err_create:
   pthread_cond_destroy (&t->cond); // destroy cond var, ignore possible error
err_cond:
   __rt_thst.next--; // release the handle
   return ret;
}

int   _rt_pthread_detach(pthread_t t)
{
   return -1;
}
int   _rt_pthread_equal(pthread_t t1, pthread_t t2)
{
   return -1;
}
int   _rt_pthread_join(pthread_t t, void **arg)
{
   return -1;
}
void  _rt_pthread_exit(void *retval)
{
   // log the _THEXIT event
   TRACE0 (_THEXIT);

   // exit protocol: we release the cs mutex
   _rt_thread_protocol_yield (__rt_thst.current);

   // terminate this thread and return retval
   pthread_exit (retval);
}
pthread_t _rt_pthread_self (void)
{
   return __rt_thst.current->tid;
}

// attributes for mutexes
int   _rt_pthread_mutexattr_destroy(pthread_mutexattr_t *);
int   _rt_pthread_mutexattr_getprioceiling(const pthread_mutexattr_t *, int *);
int   _rt_pthread_mutexattr_getprotocol(const pthread_mutexattr_t *, int *);
int   _rt_pthread_mutexattr_getpshared(const pthread_mutexattr_t *, int *);
int   _rt_pthread_mutexattr_gettype(const pthread_mutexattr_t *, int *);
int   _rt_pthread_mutexattr_init(pthread_mutexattr_t *);
int   _rt_pthread_mutexattr_setprioceiling(pthread_mutexattr_t *, int);
int   _rt_pthread_mutexattr_setprotocol(pthread_mutexattr_t *, int);
int   _rt_pthread_mutexattr_setpshared(pthread_mutexattr_t *, int);
int   _rt_pthread_mutexattr_settype(pthread_mutexattr_t *, int);

// mutexes
int   _rt_pthread_mutex_destroy(pthread_mutex_t *);
int   _rt_pthread_mutex_getprioceiling(const pthread_mutex_t *, int *);
int   _rt_pthread_mutex_init(pthread_mutex_t *, const pthread_mutexattr_t *);
int   _rt_pthread_mutex_lock(pthread_mutex_t *);
int   _rt_pthread_mutex_setprioceiling(pthread_mutex_t *, int, int *);
int   _rt_pthread_mutex_trylock(pthread_mutex_t *);
int   _rt_pthread_mutex_unlock(pthread_mutex_t *);

// attributes for conditional variables
int   _rt_pthread_condattr_destroy(pthread_condattr_t *);
int   _rt_pthread_condattr_getpshared(const pthread_condattr_t *, int *);
int   _rt_pthread_condattr_init(pthread_condattr_t *);
int   _rt_pthread_condattr_setpshared(pthread_condattr_t *, int);

// conditional variables
int   _rt_pthread_cond_broadcast(pthread_cond_t *);
int   _rt_pthread_cond_destroy(pthread_cond_t *);
int   _rt_pthread_cond_init(pthread_cond_t *, const pthread_condattr_t *);
int   _rt_pthread_cond_signal(pthread_cond_t *);
int   _rt_pthread_cond_timedwait(pthread_cond_t *, pthread_mutex_t *, const struct timespec *);
int   _rt_pthread_cond_wait(pthread_cond_t *, pthread_mutex_t *);

// thread cancellation
int   _rt_pthread_cancel(pthread_t);
void  _rt_pthread_cleanup_push(void(*)(void*), void *);
void  _rt_pthread_cleanup_pop(int);
int   _rt_pthread_setcancelstate(int, int *);
int   _rt_pthread_setcanceltype(int, int *);
void  _rt_pthread_testcancel(void);

// parameters about thread scheduling
int   _rt_pthread_getconcurrency(void);
int   _rt_pthread_setconcurrency(int);
int   _rt_pthread_getschedparam(pthread_t, int *, struct sched_param *);
int   _rt_pthread_setschedparam(pthread_t, int , const struct sched_param *);

// TLS
int   _rt_pthread_key_create(pthread_key_t *, void (*)(void *));
int   _rt_pthread_key_delete(pthread_key_t);
void *_rt_pthread_getspecific(pthread_key_t);
int   _rt_pthread_setspecific(pthread_key_t, const void *);

// miscellanea
int   _rt_pthread_once(pthread_once_t *, void (*)(void));

// read-write lock attributes
int   _rt_pthread_rwlockattr_destroy(pthread_rwlockattr_t *);
int   _rt_pthread_rwlockattr_getpshared(const pthread_rwlockattr_t *, int *);
int   _rt_pthread_rwlockattr_init(pthread_rwlockattr_t *);
int   _rt_pthread_rwlockattr_setpshared(pthread_rwlockattr_t *, int);

// read-write locks
int   _rt_pthread_rwlock_destroy(pthread_rwlock_t *);
int   _rt_pthread_rwlock_init(pthread_rwlock_t *, const pthread_rwlockattr_t *);
int   _rt_pthread_rwlock_rdlock(pthread_rwlock_t *);
int   _rt_pthread_rwlock_tryrdlock(pthread_rwlock_t *);
int   _rt_pthread_rwlock_trywrlock(pthread_rwlock_t *);
int   _rt_pthread_rwlock_unlock(pthread_rwlock_t *);
int   _rt_pthread_rwlock_wrlock(pthread_rwlock_t *);

void  _rt_thread_init (void)
{
   __rt_thst.next = 0;
}

void *_rt_thread_start (void *arg)
{
   struct rt_thread *t = (struct rt_thread *) arg;
   void *ret;

   printf ("stid: rt: threading: start: t%d: starting!\n", TID (t));

   // start protocol: we wait to get our context switch
   _rt_thread_protocol_wait (t);

   // we run the function provided, with the good argument
   ret = t->start (arg);

   // exit the thread through one unique place
   _rt_pthread_exit (ret);

   return ret; // unreachable
}

void _rt_thread_protocol_wait (struct rt_thread *t)
{
   int ret;

   // lock on the global mutex
   ret = pthread_mutex_lock (&__rt_thst.m);
   if (ret != 0) goto err_panic;

   while (1)
   {
      printf ("stid: rt: threading: proto: t%d: acquired cs lock\n", TID (t));
      TRACE3 (_THCTXSW, TID (t));
      __rt_thst.current = t;
      return;
      // FIXME - if first event or replay == CS or ...; then return
      // else pthread_cond_wait (&t->cond, &__rt_thst.m);

      ASSERT (0);
   }

err_panic :
   FLPRINT ("error: t%d: acquiring internal mutex: %s", TID (t), strerror (ret));
   _rt_end (255);
}

void _rt_thread_protocol_yield (struct rt_thread *t)
{
   int ret;

   // FIXME - if something about the events, then signal

   // unlock on the global mutex
   ret = pthread_mutex_unlock (&__rt_thst.m);
   if (ret != 0) goto err_panic;

   printf ("stid: rt: threading: proto: t%d: released cs lock\n", TID (t));
   //return;

err_panic :
   FLPRINT ("error: t%d: releasing internal mutex: %s", TID (t), strerror (ret));
   _rt_end (255);
}

