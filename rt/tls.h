#ifndef _RT_TLS_H_
#define _RT_TLS_H_

// Thread-local block. Pointer "block" points to a memory region of "size"
// bytes where all thread-local variables of the thread are stored. The block is
// initialized by copying a master, containing the initial contents (in turn,
// emitted by the stid::Executor / Instrumenter) of the thread-local variables.
struct rt_tls
{
   void *block;
   size_t size;
};

struct rt_tcb;

// init the __state.tlsinit from the rt struct
void __rt_tls_init ();

// me->tls->block = malloc() + copy from __state.tlsinit
int  __rt_tls_thread_start (struct rt_tcb *me);

// free me->tls->block
void  __rt_tls_thread_term (struct rt_tcb *me);


#endif
