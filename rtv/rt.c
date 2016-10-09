
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <ctype.h>
#include <assert.h>
#include <string.h>

#include "rt/rt.h"

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


static void _rt_header ()
{
   printf ("what             addr            value\n");
   printf ("==== ================ ================\n");
}

static inline void _rt_check_limits_addr (const void *ptr)
{
   // if the memory access is offlimits, we finish execution
   if ((uint64_t) ptr < memstart || (uint64_t) ptr >= memend) _rt_end ();
}

static inline void _rt_check_trace_capacity ()
{
   // only one memory access (for the ptr)
   if ((uint64_t) rt->trace.evptr == evend) _rt_end ();
}

// memory loads
//
void _rt_load8 (uint8_t *addr, uint8_t v)
{
   _rt_check_limits_addr ((void*) addr);

   printf ("ld8  %16p %16x '%s' ev,addr,val = %p,%p,%p\n",
         addr, v, _rt_quote (v),
         rt->trace.evptr,
         rt->trace.addrptr,
         rt->trace.valptr);

   _rt_check_trace_capacity ();
   *rt->trace.evptr++   = LD8;
   *rt->trace.addrptr++ = (uint64_t) addr;
   *rt->trace.valptr++  = v;
}

void _rt_load16 (uint16_t *addr, uint16_t v)
{
   printf ("ld16 %16p %16"PRIx16" ev,addr,val = %p,%p,%p\n",
         addr, v,
         rt->trace.evptr,
         rt->trace.addrptr,
         rt->trace.valptr);

   _rt_check_trace_capacity ();
   *rt->trace.evptr++   = LD16;
   *rt->trace.addrptr++ = (uint64_t) addr;
   *rt->trace.valptr++  = v;
}

void _rt_load32 (uint32_t *addr, uint32_t v)
{
   printf ("ld32 %16p %16"PRIx32" ev,addr,val = %p,%p,%p\n",
         addr, v,
         rt->trace.evptr,
         rt->trace.addrptr,
         rt->trace.valptr);
   _rt_check_trace_capacity ();
   *rt->trace.evptr++   = LD32;
   *rt->trace.addrptr++ = (uint64_t) addr;
   *rt->trace.valptr++  = v;
}

void _rt_load64 (uint64_t *addr, uint64_t v)
{
   printf ("ld64 %16p %16"PRIx64"\n", addr, v);
}

// memory stores
//
void _rt_store8 (uint8_t *addr, uint8_t v)
{
   printf ("st8  %16p %16x '%s'\n", addr, v, _rt_quote (v));
}
void _rt_store16 (uint16_t *addr, uint16_t v)
{
   printf ("st16 %16p %16"PRIx16"\n", addr, v);
}
void _rt_store32 (uint32_t *addr, uint32_t v)
{
   printf ("st32 %16p %16"PRIx32"\n", addr, v);
}
void _rt_store64 (uint64_t *addr, uint64_t v)
{
   printf ("st64 %16p %16"PRIx64"\n", addr, v);
}

// others
//
void _rt_allo (uint8_t *addr, uint32_t size)
{
   printf ("allo %16p %16x (stack)\n", addr, size);
}
void _rt_mllo (uint8_t *addr, uint64_t size)
{
   printf ("mllo %16p %16lx (heap)\n", addr, size);
}
void _rt_rllo (uint8_t *old, uint8_t *neww, uint64_t size)
{
   printf ("rllo %16p %16p %16lx (heap)\n", old, neww, size);
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
   return malloc (size);
}

void  _rt_free    (void *ptr)
{
   return free (ptr);
}

void *_rt_realloc (void *ptr, size_t size)
{
   return realloc (ptr, size);
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

   printf ("dpu: rt: main: memstart %p\n", (void*) memstart);
   printf ("dpu: rt: main: memend   %p\n", (void*) memend);
   s = memend - memstart;
   printf ("dpu: rt: main: %luB (%luM)\n", s, s / (1024 * 1024));
   printf ("dpu: rt: main: rt->trace.evstart %p\n", (void*) rt->trace.evstart);
   printf ("dpu: rt: main: rt->trace.evptr   %p\n", (void*) rt->trace.evptr);
   printf ("dpu: rt: main: rt->trace.evend   %p\n", (void*) rt->trace.evend);
   s = rt->trace.evend - rt->trace.evstart;
   printf ("dpu: rt: main: %lu ev (%lu Mev)\n", s, s / (1024 * 1024));

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

   printf ("dpu: rt: main: myargc %d myargv %p, myenv %p (%d)\n",
         argc, myargv, myenv, n);
   for (i = 0; i < argc; i++)
      printf ("dpu: rt: main: argv[%2i] %16p '%s'\n", i, myargv[i], myargv[i]);
   for (i = 0; i <= n; i++)
      printf ("dpu: rt: main:  env[%2i] %16p '%s'\n", i, myenv[i], myenv[i]);

   // call main
   printf ("dpu: rt: main: calling user's main...\n");
   _rt_header ();
   ret = main (argc, myargv, myenv);

	// exit
   printf ("dpu: rt: main: returned %d\n", ret);
   return ret;
}

void _rt_panic ()
{
   printf ("rt: panic!!\n");
   while (1);
}

