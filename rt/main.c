
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
#define TRACE0(e) \
      *rt->trace.evptr++   = e;
// event + address
#define TRACE1(e,addr) \
      _rt_debug_trace1 (e, addr); \
      *rt->trace.evptr++   = e; \
      *rt->trace.addrptr++ = (uint64_t) addr;
// event + address + val
#define TRACE2(e,addr,val) \
      _rt_debug_trace2 (e, addr, val); \
      *rt->trace.evptr++   = e; \
      *rt->trace.addrptr++ = (uint64_t) addr; \
      *rt->trace.valptr++  = val;
// event + id
#define TRACE3(e,id) \
      _rt_debug_trace3 (e, id); \
      *rt->trace.evptr++   = e; \
      *rt->trace.idptr++   = id;
#define TRACE128(e,addr,val) \
      _rt_debug_trace128 (e, addr, val); \
      *rt->trace.evptr++   = e; \
      *rt->trace.addrptr++ = (uint64_t) addr; \
      memcpy (rt->trace.valptr, &(val), 16); \
      rt->trace.valptr += 2;

#define UNITS_UNIT(s) \
      (s) < 2048 ? "B" : \
         (s) / 1024 < 1024 ? "K" : \
            (s) / (1024 * 1024) < 1024 ? "M" : "G"

#define UNITS_SIZE(s) \
      (s) < 2048 ? (s) : \
         (s) / 1024 < 1024 ? (s) / 1024 : \
            (s) / (1024 * 1024) < 1024 ? (s) / (1024 * 1024) : \
               (s) / (size_t) (1024 * 1024 * 1024)

// this data will be filled by the host when instrumenting the code
static struct rt * const rt; // stored in the Executor object, in the host
static const uint64_t memstart;
static const uint64_t memend;
static const uint64_t evend;

void _rt_breakme () {}

// we need to redefine the errno macro for us, since instrumentation will not
// happen inside of the _rt_* functions
#ifdef errno
#undef errno
#endif
#define errno (*_rt___errno_location ())

#include "events.c"
#include "mm.c"
#include "pthread.c"
#include "unistd.c"

