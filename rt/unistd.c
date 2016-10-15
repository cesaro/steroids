
#include <unistd.h>
#include "pthread.h"

unsigned int _rt_sleep (unsigned int sec)
{
   struct rt_tcb *me = __rt_thst.current;
   unsigned ret;

   //_rt_thread_protocol_yield (me);
   ret = sleep (sec);
   //_rt_thread_protocol_wait (me);
   return ret;
}

int _rt_usleep (useconds_t us)
{
   struct rt_tcb *me = __rt_thst.current;
   unsigned ret;

   //_rt_thread_protocol_yield (me);
   ret = usleep (us);
   //_rt_thread_protocol_wait (me);
   return ret;
}

