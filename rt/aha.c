
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <ctype.h>
#include <assert.h>
#include <string.h>

#include "rt.h"

void _rt_breakme () {}

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

static const char *_rt_quote (char c)
{
   static char str[5];

   if (c == '\n') return "\\n";
   if (c == '\0') return "\\0";
   if (c == '\r') return "\\r";
   if (c == '\t') return "\\t";
   if (isprint (c))
   {
      str[0] = c;
      str[1] = 0;
      return str;
   }
   sprintf (str, "\\x%02x", c);
   return str;
}

static const char *_rt_ev_to_str (enum eventtype e)
{
   switch (e)
   {
   // loads
   case RD8       : return "RD8    ";
   case RD16      : return "RD16   ";
   case RD32      : return "RD32   ";
   case RD64      : return "RD64   ";
   // stores
   case WR8       : return "WR8    ";
   case WR16      : return "WR16   ";
   case WR32      : return "WR32   ";
   case WR64      : return "WR64   ";
   // memory management
   case ALLO      : return "ALLO   ";
   case MLLO      : return "MLLO   ";
   case FREE      : return "FREE   ";
   case CALL      : return "CALL   ";
   case RET       : return "RET    ";
   // threads
   case THCREAT   : return "THCREAT";
   case THJOIN    : return "THJOIN ";
   case THSW      : return "THSW   ";
   // locks
   case MTXINIT   : return "MTXINIT";
   case MTXLOCK   : return "MTXLOCK";
   case MTXUNLK   : return "MTXUNLK";
   // misc
   case _EV_LAST  : return "_EV_LAST";
   }
}

static void _rt_debug_header ()
{
   printf ("stid: rt: what                addr            value comments\n");
   printf ("stid: rt: ======= ================ ================ ======================\n");
}

static void _rt_debug_trace0 (enum eventtype e)
{
   printf ("stid: rt: %s", _rt_ev_to_str (e));
}

static void _rt_debug_trace1 (enum eventtype e, void *addr)
{
   printf ("stid: rt: %s %16p\n",
         _rt_ev_to_str (e), addr);
}

static void _rt_debug_trace2 (enum eventtype e, void *addr, uint64_t v)
{
   printf ("stid: rt: %s %16p %16lx %s\n",
         _rt_ev_to_str (e), addr, v, _rt_quote (v));
}

static void _rt_debug_trace3 (enum eventtype e, uint16_t v)
{
   printf ("stid: rt: %s                  %16x %s\n",
         _rt_ev_to_str (e), v, _rt_quote (v));
}

static inline void _rt_check_limits_addr (const void *ptr, enum eventtype e)
{
   // if the memory access is offlimits, we finish execution
   if ((uint64_t) ptr < memstart || (uint64_t) ptr >= memend)
   {
      printf ("stid: rt: out-of-memory access: event %d addr %p\n", e, ptr);
      _rt_end ();
   }
}

static inline void _rt_check_trace_capacity ()
{
   // if the buffer space is exhausted, we finish
   // observe that this requires only one memory access (for evptr)
   if ((uint64_t) rt->trace.evptr == evend) _rt_end ();
}

// memory loads
//
void _rt_load8 (uint8_t *addr, uint8_t v)
{
   TRACE2 (RD8, addr, v);
   _rt_check_limits_addr ((void*) addr, RD8);
   _rt_check_trace_capacity ();
}
void _rt_load16 (uint16_t *addr, uint16_t v)
{
   TRACE2 (RD16, addr, v);
   _rt_check_limits_addr ((void*) addr, RD16);
   _rt_check_trace_capacity ();
}
void _rt_load32 (uint32_t *addr, uint32_t v)
{
   TRACE2 (RD32, addr, v);
   _rt_check_limits_addr ((void*) addr, RD32);
   _rt_check_trace_capacity ();
}
void _rt_load64 (uint64_t *addr, uint64_t v)
{
   TRACE2 (RD64, addr, v);
   _rt_check_limits_addr ((void*) addr, RD64);
   _rt_check_trace_capacity ();
}

// memory stores
//
void _rt_store8 (uint8_t *addr, uint8_t v)
{
   TRACE2 (WR8, addr, v);
   _rt_check_limits_addr ((void*) addr, WR8);
   _rt_check_trace_capacity ();
}
void _rt_store16 (uint16_t *addr, uint16_t v)
{
   TRACE2 (WR16, addr, v);
   _rt_check_limits_addr ((void*) addr, WR16);
   _rt_check_trace_capacity ();
}
void _rt_store32 (uint32_t *addr, uint32_t v)
{
   TRACE2 (WR32, addr, v);
   _rt_check_limits_addr ((void*) addr, WR32);
   _rt_check_trace_capacity ();
}
void _rt_store64 (uint64_t *addr, uint64_t v)
{
   TRACE2 (WR64, addr, v);
   _rt_check_limits_addr ((void*) addr, WR64);
   _rt_check_trace_capacity ();
}

// memory management
//
void _rt_allo (uint8_t *addr, uint32_t size)
{
   _rt_debug_trace2 (ALLO, addr, size);
}
void _rt_mllo (uint8_t *addr, uint64_t size)
{
   _rt_debug_trace2 (MLLO, addr, size);
}
void _rt_rllo (uint8_t *old, uint8_t *neww, uint64_t size)
{
   _rt_debug_trace1 (FREE, old);
   _rt_debug_trace2 (MLLO, neww, size);
}
void _rt_fre (uint8_t *addr)
{
   printf ("free %16p    (heap)\n", addr);
}

void _rt_call (uint32_t id)
{
   printf ("call                  %16d\n", id);
}
void _rt_ret (uint32_t id)
{
   printf ("ret                   %16d\n", id);
}

void *_rt_malloc  (size_t size)
{
   void *ptr;
   ptr = malloc (size);
   printf ("stid: rt: malloc: size %zu ret %p\n", size, ptr);
   return ptr;
}

void  _rt_free    (void *ptr)
{
   printf ("stid: rt: free: ptr %p\n", ptr);
   return free (ptr);
}

void *_rt_realloc (void *ptr, size_t size)
{
   void *ret;
   ret = realloc (ptr, size);
   printf ("stid: rt: malloc: ptr %p size %zu ret %p\n", ptr, size, ret);
   return ret;
}

int _rt_main (int argc, const char * const *argv, const char * const *env)
{
   int ret;
	int i, n;
	const char * const *v;
   uint64_t s;

   // assert that global const variables equal corresponding ones in the rt
   // structure
   ASSERT (rt->memstart == memstart);
   ASSERT (rt->memend == memend);
   ASSERT (rt->memsize == (memend - memstart));
   ASSERT ((uint64_t) rt->trace.evend == evend); // xxxxxxxxxxxxxx
   ASSERT (rt->trace.evptr == rt->trace.evstart);

   printf ("stid: rt: main: I feel fantastic... I feel the PUMP!\n");
   printf ("stid: rt: main: memstart %p\n", (void*) memstart);
   printf ("stid: rt: main: memend   %p\n", (void*) memend);
   s = memend - memstart;
   printf ("stid: rt: main: %luB (%luM)\n", s, s / (1024 * 1024));
   printf ("stid: rt: main: rt->trace.evstart %p\n", (void*) rt->trace.evstart);
   printf ("stid: rt: main: rt->trace.evptr   %p\n", (void*) rt->trace.evptr);
   printf ("stid: rt: main: rt->trace.evend   %p\n", (void*) rt->trace.evend);
   s = rt->trace.evend - rt->trace.evstart;
   printf ("stid: rt: main: %lu ev (%lu Mev)\n", s, s / (1024 * 1024));

	// determine number of environment variables
	for (n = 0, v = env; *v; ++n, ++v)
	;

   // copy arguments into our stack and heap
   char *myargv[argc];
   char *myenv[n+1];
   for (i = 0; i < argc; i++)
   {
      myargv[i] = _rt_malloc (strlen (argv[i]) + 1);
      strcpy (myargv[i], argv[i]);
   }
   for (i = 0; i < n; i++)
   {
      myenv[i] = _rt_malloc (strlen (env[i]) + 1);
      strcpy (myenv[i], env[i]);
   }
   myenv[n] = 0;

   printf ("stid: rt: main: myargc %d myargv %p, myenv %p (%d)\n",
         argc, myargv, myenv, n);
   for (i = 0; i < argc; i++)
      printf ("stid: rt: main: argv[%2i] %16p '%s'\n", i, myargv[i], myargv[i]);
   for (i = 0; i <= n; i++)
      printf ("stid: rt: main:  env[%2i] %16p '%s'\n", i, myenv[i], myenv[i]);

   // call main
   printf ("stid: rt: main: calling user's main...\n");
   _rt_debug_header ();
   ret = main (argc, myargv, myenv);

	// exit
   printf ("stid: rt: main: returned %d\n", ret);
   return ret;
}

void _rt_save_host_rsp (uint64_t rsp)
{
   rt->host_rsp = rsp;
}

uint64_t _rt_get_host_rsp ()
{
   return rt->host_rsp;
}

void _rt_panic ()
{
   printf ("stid: rt: panic!!\n");
   while (1);
}

