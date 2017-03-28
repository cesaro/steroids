
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <inttypes.h>
#include <ctype.h>
#include <errno.h>
#include <assert.h>
#include <string.h>

#include "rt.h"
#include "pthread.h"
#include "debug.h"

// only the event
#define TRACE0(e) { \
      __rt_debug_trace0 (e); \
      *rt->trace.evptr++   = e; \
      }
// event + address
#define TRACE1(e,addr) { \
      __rt_debug_trace1 (e, addr); \
      *rt->trace.evptr++   = e; \
      *rt->trace.addrptr++ = (uint64_t) addr; \
      }
// event + address + val
#define TRACE2(e,addr,val) { \
      __rt_debug_trace2 (e, addr, val); \
      *rt->trace.evptr++   = e; \
      *rt->trace.addrptr++ = (uint64_t) addr; \
      *rt->trace.valptr++  = val; \
      }
// event + id
#define TRACE3(e,id) { \
      __rt_debug_trace3 (e, id); \
      *rt->trace.evptr++   = e; \
      *rt->trace.idptr++   = id; \
      }
#define TRACE128(e,addr,val) { \
      __rt_debug_trace128 (e, addr, val); \
      *rt->trace.evptr++   = e; \
      *rt->trace.addrptr++ = (uint64_t) addr; \
      memcpy (rt->trace.valptr, &(val), 16); \
      rt->trace.valptr += 2; \
      }

#define UNITS_UNIT(s) \
      (s) < 2048 ? "B" : \
         (s) / 1024 < 1024 ? "K" : \
            (s) / (1024 * 1024) < 1024 ? "M" : "G"

#define UNITS_SIZE(s) \
      (s) < 2048 ? (s) : \
         (s) / 1024 < 1024 ? (s) / 1024 : \
            (s) / (1024 * 1024) < 1024 ? (s) / (1024 * 1024) : \
               (s) / (size_t) (1024 * 1024 * 1024)

#define STRACE(what,fmt,args...) { \
   if (rt->strace.what) \
      printf ("t%d: " fmt "\n", TID(__state.current), ##args); \
   }

// this data will be filled by the host when instrumenting the code
static struct rt * const rt; // stored in the Executor object, in the host
static const uint64_t memstart;
static const uint64_t memend;
static const uint64_t evend;

uint64_t __rt_force_memstart_present ()
{
   // since this function is public, the compiler cannot remove memstart from
   // the object file
   // nor it will remove struct rt (at least in theory :/)
   struct rt r;
   return memstart + memend + evend + (uint64_t) &r;
}

void breakme () {}

// the user's main function, epic :)
// this should be in rt/rt.h, but will break compilation of other projects, as
// it becomes visible; see README.md for comments
int main (int argc, char **argv, char **env);

// runtime, thread control and scheduling, replay support, tls
#include "events.c"
#include "thread-sched.c"
#include "pthread.c"
#include "tls.c"

// libc function hooks
#include "libc/proc.c"
#include "libc/fs.c"
#include "libc/mm.c"
#include "libc/stdio.c"
#include "libc/misc.c"
#include "libc/extvars.c" // libc variables

