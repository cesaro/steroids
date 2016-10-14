
#ifndef _RT_PTHREAD_H_
#define _RT_PTHREAD_H_

// standard: http://pubs.opengroup.org/onlinepubs/7908799/xsh/pthread.h.html

#include <pthread.h>

#define RT_MAX_THREADS 128

struct rt_thread
{
   // flags, see below
   unsigned flags;
   // handle for the linux threads library
   pthread_t tid;
   // conditional variable, for the WAIT protocol when in replay mode
   pthread_cond_t cond;

   // the arguments of pthread_create
   void *(*start)(void *);
   void *arg;
};

// thread attributes
int   _rt_pthread_attr_destroy(pthread_attr_t *);
int   _rt_pthread_attr_getdetachstate(const pthread_attr_t *, int *);
int   _rt_pthread_attr_getguardsize(const pthread_attr_t *, size_t *);
int   _rt_pthread_attr_getinheritsched(const pthread_attr_t *, int *);
int   _rt_pthread_attr_getschedparam(const pthread_attr_t *, struct sched_param *);
int   _rt_pthread_attr_getschedpolicy(const pthread_attr_t *, int *);
int   _rt_pthread_attr_getscope(const pthread_attr_t *, int *);
int   _rt_pthread_attr_getstackaddr(const pthread_attr_t *, void **);
int   _rt_pthread_attr_getstacksize(const pthread_attr_t *, size_t *);
int   _rt_pthread_attr_init(pthread_attr_t *);
int   _rt_pthread_attr_setdetachstate(pthread_attr_t *, int);
int   _rt_pthread_attr_setguardsize(pthread_attr_t *, size_t);
int   _rt_pthread_attr_setinheritsched(pthread_attr_t *, int);
int   _rt_pthread_attr_setschedparam(pthread_attr_t *, const struct sched_param *);
int   _rt_pthread_attr_setschedpolicy(pthread_attr_t *, int);
int   _rt_pthread_attr_setscope(pthread_attr_t *, int);
int   _rt_pthread_attr_setstackaddr(pthread_attr_t *, void *);
int   _rt_pthread_attr_setstacksize(pthread_attr_t *, size_t);

// thread creation / joining / exiting
int   _rt_pthread_create(pthread_t *, const pthread_attr_t *, void *(*)(void *), void *);
int   _rt_pthread_detach(pthread_t);
int   _rt_pthread_equal(pthread_t, pthread_t);
int   _rt_pthread_join(pthread_t, void **);
void  _rt_pthread_exit(void *);
pthread_t
      _rt_pthread_self(void);

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
int   _rt_pthread_condattr_init(pthread_condattr_t *);
int   _rt_pthread_condattr_destroy(pthread_condattr_t *);
int   _rt_pthread_condattr_getpshared(const pthread_condattr_t *, int *);
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

// internal, for us
void  _rt_thread_init (void);
void *_rt_thread_start (void *arg);
void _rt_thread_protocol_wait (struct rt_thread *t);
void _rt_thread_protocol_yield (struct rt_thread *t);

#endif
