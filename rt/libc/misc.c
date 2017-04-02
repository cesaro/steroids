
#include <errno.h>

#include "misc.h"

unsigned int _rt_sleep (unsigned int sec)
{
   if (rt->flags.dosleep)
   {
      STRACE (proc, "sleep (sec=%u)", sec);
      return sleep (sec);
   }
   else
   {
      STRACE (others, "sleep (sec=%u) [skipping]", sec);
      return sec;
   }
}

int _rt_usleep (useconds_t us)
{
   if (rt->flags.dosleep)
   {
      STRACE (others, "usleep (us=%u)", us);
      return usleep (us);
   }
   else
   {
      STRACE (others, "usleep (us=%u) [EINTR]", us);
      errno = EINTR;
      return -1;
   }
}


int _rt_clock_nanosleep (clockid_t id, int flags, const struct timespec *req, struct timespec *rem)
{
   if (rt->flags.dosleep)
   {
      STRACE (others, "clock_nanosleep (id=%d, flags=%#x, req=%p, rem=%p)",
            id, flags, req, rem);
      return clock_nanosleep (id, flags, req, rem);
   }
   else
   {
      STRACE (others, "clock_nanosleep (id=%d, flags=%#x, req=%p, rem=%p) [EINTR]",
            id, flags, req, rem);
      if (rem && ! (flags & TIMER_ABSTIME)) *rem = *req;
      return EINTR;
   }
}

int *_rt___errno_location ()
{
   // for the time being we let the guest access directly glibc's errno, since
   // we are not enforcing bounds on the memory accesses
   return __errno_location (); // in glibc !!
}

void _rt___assert_fail (const char *__assertion, const char *__file,
			   unsigned int __line, const char *__function)
{
   printf ("stid: rt: assert-fail: called!\n");
   printf ("%s:%d: %s: Assertion `%s' failed. Aborting.\n",
         __file, __line, __function, __assertion);
   _rt_abort ();
}
