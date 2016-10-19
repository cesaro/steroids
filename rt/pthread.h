
#ifndef _RT_PTHREAD_H_
#define _RT_PTHREAD_H_

// According to
// The Open Group Base Specifications Issue 7
// IEEE Std 1003.1-2008, 2016 Edition
//
// http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/pthread.h.html

#include <pthread.h>
#include "lsd.h"

// Thread Control Block
struct rt_tcb
{
   // flags, see below
   struct {
      unsigned alive : 1;
      unsigned detached : 1;
   } flags;
   // handle for the NPTL library
   pthread_t tid;
   // conditional variable, for the WAIT protocol when in replay mode
   pthread_cond_t cond;

   // the arguments of pthread_create and return value of the thread
   void *(*start)(void *);
   void *arg;
   void *retval;

   // location of the stack
   void  *stackaddr;
   size_t stacksize;

   // the list of mutexes currently owned by the thread
   pthread_mutex_t *ownedmut[RT_MAX_OWNED_MUTEXES];
   unsigned ownedmut_size;
};

struct rt_mut
{
   struct lsd node;
};

// thread attributes
int   _rt_pthread_attr_init(pthread_attr_t *);
int   _rt_pthread_attr_destroy(pthread_attr_t *);
int   _rt_pthread_attr_getdetachstate(const pthread_attr_t *, int *);
int   _rt_pthread_attr_setdetachstate(pthread_attr_t *, int);
int   _rt_pthread_attr_getguardsize(const pthread_attr_t *restrict, size_t *restrict);
int   _rt_pthread_attr_getinheritsched(const pthread_attr_t *restrict, int *restrict);
int   _rt_pthread_attr_getschedparam(const pthread_attr_t *restrict, struct sched_param *restrict);
int   _rt_pthread_attr_getschedpolicy(const pthread_attr_t *restrict, int *restrict);
int   _rt_pthread_attr_getscope(const pthread_attr_t *restrict, int *restrict);
int   _rt_pthread_attr_getstack(const pthread_attr_t *restrict, void **restrict, size_t *restrict);
int   _rt_pthread_attr_setstack(pthread_attr_t *, void *, size_t);
int   _rt_pthread_attr_getstacksize(const pthread_attr_t *restrict, size_t *restrict);
int   _rt_pthread_attr_setstacksize(pthread_attr_t *, size_t);
int   _rt_pthread_attr_setguardsize(pthread_attr_t *, size_t);
int   _rt_pthread_attr_setinheritsched(pthread_attr_t *, int);
int   _rt_pthread_attr_setschedparam(pthread_attr_t *restrict, const struct sched_param *restrict);
int   _rt_pthread_attr_setschedpolicy(pthread_attr_t *, int);
int   _rt_pthread_attr_setscope(pthread_attr_t *, int);

// thread creation / joining / exiting
int   _rt_pthread_create(pthread_t *restrict, const pthread_attr_t *restrict, void *(*)(void*), void *restrict);
void  _rt_pthread_exit(void *);
int   _rt_pthread_join(pthread_t, void **);
int   _rt_pthread_detach(pthread_t);
int   _rt_pthread_equal(pthread_t, pthread_t);
pthread_t _rt_pthread_self(void);

// thread cancellation
int   _rt_pthread_cancel(pthread_t);
int   _rt_pthread_setcancelstate(int, int *);
int   _rt_pthread_setcanceltype(int, int *);
void  _rt_pthread_testcancel(void);
void  _rt_pthread_cleanup_push(void (*routine)(void *arg), void *arg);
void  _rt_pthread_cleanup_pop(int);

// attributes for mutexes
int   _rt_pthread_mutexattr_init(pthread_mutexattr_t *);
int   _rt_pthread_mutexattr_destroy(pthread_mutexattr_t *);
int   _rt_pthread_mutexattr_getprioceiling( const pthread_mutexattr_t *restrict, int *restrict);
int   _rt_pthread_mutexattr_getprotocol(const pthread_mutexattr_t *restrict, int *restrict);
int   _rt_pthread_mutexattr_getpshared(const pthread_mutexattr_t *restrict, int *restrict);
int   _rt_pthread_mutexattr_getrobust(const pthread_mutexattr_t *restrict, int *restrict);
int   _rt_pthread_mutexattr_gettype(const pthread_mutexattr_t *restrict, int *restrict);
int   _rt_pthread_mutexattr_setprioceiling(pthread_mutexattr_t *, int);
int   _rt_pthread_mutexattr_setprotocol(pthread_mutexattr_t *, int);
int   _rt_pthread_mutexattr_setpshared(pthread_mutexattr_t *, int);
int   _rt_pthread_mutexattr_setrobust(pthread_mutexattr_t *, int);
int   _rt_pthread_mutexattr_settype(pthread_mutexattr_t *, int);

// mutexes
int   _rt_pthread_mutex_init(pthread_mutex_t *restrict, const pthread_mutexattr_t *restrict);
int   _rt_pthread_mutex_destroy(pthread_mutex_t *);
int   _rt_pthread_mutex_getprioceiling(const pthread_mutex_t *restrict, int *restrict);
int   _rt_pthread_mutex_setprioceiling(pthread_mutex_t *restrict, int, int *restrict);
int   _rt_pthread_mutex_consistent(pthread_mutex_t *);
int   _rt_pthread_mutex_lock(pthread_mutex_t *);
int   _rt_pthread_mutex_unlock(pthread_mutex_t *);
int   _rt_pthread_mutex_timedlock(pthread_mutex_t *restrict, const struct timespec *restrict);
int   _rt_pthread_mutex_trylock(pthread_mutex_t *);

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

// internal
void  _rt_thread_init (void);
void  _rt_thread_term (void);
void *_rt_thread_start (void *arg);
void  _rt_thread_protocol_wait (struct rt_tcb *t);
void  _rt_thread_protocol_wait_first ();
void  _rt_thread_protocol_yield (struct rt_tcb *t);
int   _rt_thread_stack_alloc (struct rt_tcb *t, pthread_attr_t *attr);
void  _rt_thread_stack_free (struct rt_tcb *t);

#endif
