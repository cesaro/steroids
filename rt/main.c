
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <ctype.h>
#include <assert.h>
#include <string.h>

#include "rt.h"

#if 1
#define ASSERT(expr) \
	{if (! (expr)) { \
		printf (__FILE__ ":%d: %s: Assertion `" #expr "' failed.\n", \
				__LINE__, __func__); \
		_rt_breakme (); \
		exit (1); \
	}}
#else
#define ASSERT(expr)
#endif

// only the event
#define TRACE0(e) \
      _rt_debug_trace0 (e); \
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

// this data will be filled by the host when instrumenting the code
static struct rt * const rt; // stored in the Executor object, in the host
static const uint64_t memstart;
static const uint64_t memend;
static const uint64_t evend;

void _rt_breakme () {}

void _rt_mm_init ();
void _rt_mm_free (void *ptr);
void *_rt_mm_malloc (size_t size);
void *_rt_mm_realloc (void *ptr, size_t size);
void *_rt_mm_calloc (size_t nelem, size_t size);

#include "events.c"
#include "mm.c"


