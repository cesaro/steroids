
#include "tls.h"
#include "thread-sched.h"

// we assume that __state is defined here (this is pretty ugly ...)

static void __rt_tls_copy (struct rt_tls *dst, struct rt_tls *src)
{
   ASSERT (src);
   ASSERT (!src->size || src->block);
   ASSERT (dst);
   memcpy (dst->block, src->block, src->size);
   dst->size = src->size;
}

void __rt_tls_init ()
{
   // we initialize the pointer in the tlsinit block to the memory region given
   // in the rt structure; this region is read only
   __state.tlsinit.block = rt->tdata.begin;
   __state.tlsinit.size = rt->tdata.size;
}

int  __rt_tls_thread_start (struct rt_tcb *me)
{
   void *ptr;

   // we clone the initial TLS given with the program, creating a copy for this
   // thread
   ptr = _rt_malloc_uninitialized (__state.tlsinit.size);
   if (__state.tlsinit.size && !ptr) return ENOMEM;
   me->tls.block = ptr;
   __rt_tls_copy (&me->tls, &__state.tlsinit);
   _printf ("stid: rt: tls: start me=t%u me.tls.{block=%p size=%zu} tlsinit.{block=%p, size=%zu}\n",
         TID (me), me->tls.block, me->tls.size, __state.tlsinit.block, __state.tlsinit.size);
   return 0;
}

void  __rt_tls_thread_term (struct rt_tcb *me)
{
   // we release the meomry used to store the TLVs
   _rt_free (me->tls.block);
}

// returns the pointer of a given thread-local variable; a call to this function
// will be instrumented every time the program needs to get the address of the
// variable
uint8_t *__rt_tls_get_var_addr (uint32_t offset)
{
   ASSERT (__state.tlsptr == __state.current->tls.block);
   ASSERT (offset < __state.current->tls.size);
   return (uint8_t*) __state.tlsptr + offset;
}
