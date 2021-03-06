
#include "libc/mm.h"
#include "libc/stdio.h"
#include "thread-sched.h"

// this file will be #include'd in the main.c file

static const char *__rt_quote (uint64_t v)
{
   static char str[5];

   if (v >= 256) return "";
   if (v == '\n') return "\\n";
   if (v == '\0') return "\\0";
   if (v == '\r') return "\\r";
   if (v == '\t') return "\\t";
   if (! isprint (v)) return "";

   str[0] = v;
   str[1] = 0;
   return str;
}

static void __rt_debug_header ()
{
   return;
   printf ("stid: rt: what                  addr              value comments\n");
   printf ("stid: rt: ======= ================== ================== ======================\n");
}

static void __rt_debug_trace0 (uint8_t a)
{
   return;
   printf ("stid: rt: %s\n", __rt_action_to_str (a));
}

static void __rt_debug_trace1 (uint8_t a, const void *addr)
{
   return;
   printf ("stid: rt: %s %18p\n", __rt_action_to_str (a), addr);
}

static void __rt_debug_trace2 (uint8_t a, const void *addr, uint64_t v)
{
   return;
   printf ("stid: rt: %s %18p %#18lx %s\n",
         __rt_action_to_str (a), addr, v, __rt_quote (v));
}

static void __rt_debug_trace3 (uint8_t a, uint16_t v)
{
   return;
   printf ("stid: rt: %s                    %#18x %s\n",
         __rt_action_to_str (a), v, __rt_quote (v));
}

static void __rt_debug_rdwr (uint8_t a, const void *addr, uint32_t size)
{
   return; // FIXME - currently disabled
   uint64_t v;
   switch (size)
   {
   case 1 :
      v = * (const uint8_t*) addr;
      break;
   case 2 :
      v = * (const uint16_t*) addr;
      break;
   case 4 :
      v = * (const uint32_t*) addr;
      break;
   case 8 :
      v = * (const uint64_t*) addr;
      break;
   default:
      v = 0xdeadbeefdeadbeef;
   }
   printf ("stid: rt: %s %18p %#18lx %s\n",
         __rt_action_to_str (a), addr, v, __rt_quote (v));
   //fflush (stdout);
}

static inline void __rt_check_oom (const void *ptr, uint32_t size)
{
   return; // FIXME - we currently disable this
   // if the memory access is offlimits, we terminate execution
   if ((uint64_t) ptr < memstart || size + (uint64_t) ptr >= memend)
   {
      printf ("stid: rt: out-of-memory access: addr %p, size %u\n", ptr, size);
      __rt_cend (255);
   }
}

static inline void __rt_log_action (uint8_t action)
{
   // this requires 2 memory access: one load and one store of evptr
   uint8_t *ptr = rt->trace.evptr;
   if ((uint64_t) ptr == evend) __rt_cend (254);
   *ptr = action;
   rt->trace.evptr = ptr + 1;
}

static inline void __rt_log_addr (const void *addr)
{
   *rt->trace.addrptr++ = (uint64_t) addr;
}

static inline uint8_t __rt_rdwr_get_action (int isread, uint32_t size)
{
   uint8_t class;

   // we accept sizes of 1, 2, 4, or multiples of 8 from 8 to 256

   class = isread ? RT_ACTION_CLASS_RD8 : RT_ACTION_CLASS_WR8;
   switch (size)
   {
   case 1 :
      return class | 1; // 8bits
   case 2 :
      return class | 2; // 16bits
   case 4 :
      return class | 4; // 32bits
   default :
      break;
   }

   // 64bits or more
   class = isread ? RT_ACTION_CLASS_RD64 : RT_ACTION_CLASS_WR64;
   return class | ((size / 8) & RT_ACTION_ID_MASK);

   // if size == 3, 5, 6, 7, then kind == 0
   // if size >= 256, then we silently ignore part of the memory chunk
}

static inline void __rt_log_rdwr_val (const void *addr, uint32_t size)
{
   unsigned i;
   unsigned char *src = (unsigned char *) addr;
   uint64_t *dst = rt->trace.valptr;

   // 1, 2 or 4 bytes
   switch (size)
   {
   case 1 :
      *rt->trace.valptr++ = * (uint8_t*) src;
      return;
   case 2 :
      *rt->trace.valptr++ = * (uint16_t*) src;
      return;
   case 4 :
      *rt->trace.valptr++ = * (uint32_t*) src;
      return;
   default:
      break;
   }

   // 64 bit words
   size = ((size / 8) & RT_ACTION_ID_MASK) * 8;
   for (i = 0; i < size; i += 8)
   {
      *dst++ = * (uint64_t*) (src + i);
   }
   rt->trace.valptr = dst;
}


#if 0
static inline void __rt_check_trace_capacity () {}
// memory loads - FIXME, later on we should make RD into TRACE1
//
uint8_t  __rt_load8  (uint8_t  *addr)
{
   uint8_t v;
   __rt_check_oom (addr, 8);
   v = *addr;
   TRACE2 (RT_RD8, addr, v);
   __rt_check_trace_capacity ();
   return v;
}
uint16_t __rt_load16 (uint16_t *addr)
{
   uint16_t v;
   ___rt_check_oom (addr, 16);
   v = *addr;
   TRACE2 (RT_RD16, addr, v);
   __rt_check_trace_capacity ();
   return v;
}
uint32_t __rt_load32 (uint32_t *addr)
{
   uint32_t v;
   __rt_check_oom (addr, 32);
   v = *addr;
   TRACE2 (RT_RD32, addr, v);
   __rt_check_trace_capacity ();
   return v;
}
uint64_t __rt_load64 (uint64_t *addr)
{
   uint64_t v;
   __rt_check_oom (addr, 64);
   v = *addr;
   TRACE2 (RT_RD64, addr, v);
   __rt_check_trace_capacity ();
   return v;
}
float    __rt_loadf  (float *addr)
{
   float v;
   ASSERT (sizeof (float) == 4)
   __rt_check_oom (addr, 32);
   v = *addr;
   TRACE2 (RT_RD32, addr, * (uint32_t*) (void*) &v);
   __rt_check_trace_capacity ();
   return v;
}
double   __rt_loadd  (double *addr)
{
   double v;
   ASSERT (sizeof (double) == 8)
   __rt_check_oom (addr, 64);
   v = *addr;
   TRACE2 (RT_RD64, addr, * (uint64_t*) (void*) &v);
   __rt_check_trace_capacity ();
   return v;
}
long double __rt_loadld (long double *addr)
{
   long double v;
   ASSERT (sizeof (long double) == 16)
   __rt_check_oom (addr, 128);
   v = *addr;
   TRACE128 (RT_RD128, addr, v); // event, address, 2 x val
   __rt_check_trace_capacity ();
   return v;
}

void __rt_store8 (uint8_t *addr, uint8_t v)
{
   TRACE2 (_WR8, addr, v);
   __rt_check_limits_addr ((void*) addr, _WR8);
   __rt_check_trace_capacity ();
}
void __rt_store16 (uint16_t *addr, uint16_t v)
{
   TRACE2 (_WR16, addr, v);
   __rt_check_limits_addr ((void*) addr, _WR16);
   __rt_check_trace_capacity ();
}
void __rt_store32 (uint32_t *addr, uint32_t v)
{
   TRACE2 (_WR32, addr, v);
   __rt_check_limits_addr ((void*) addr, _WR32);
   __rt_check_trace_capacity ();
}
void __rt_store64 (uint64_t *addr, uint64_t v)
{
   TRACE2 (_WR64, addr, v);
   __rt_check_limits_addr ((void*) addr, _WR64);
   __rt_check_trace_capacity ();
}
void __rt_storef  (float *addr, float v)
{
   uint32_t vv = * ((uint32_t*) (void*) &v);
   TRACE2 (_WR32, addr, vv);
   __rt_check_limits_addr ((void*) addr, _WR32);
   __rt_check_trace_capacity ();
}
void __rt_stored  (double *addr, double v)
{
   uint64_t vv = * ((uint64_t*) (void*) &v);
   TRACE2 (_WR64, addr, vv);
   __rt_check_limits_addr ((void*) addr, _WR64);
   __rt_check_trace_capacity ();
}
void __rt_storeld (long double *addr, long double v)
{
   TRACE128 (_WR128, addr, v); // event, address, 2 x val
   __rt_check_limits_addr ((void*) addr, _WR128);
   __rt_check_trace_capacity ();
}
#endif

// memory stores
//
void __rt_store_pre (const void *addr, uint32_t size)
{
   if (! do_load_store) return;
   uint8_t a = __rt_rdwr_get_action (0, size);
   __rt_log_action (a);
   __rt_log_addr (addr);
   __rt_check_oom (addr, size);
}

void __rt_store_post (const void *addr, uint32_t size)
{
   if (! do_load_store) return;
   __rt_log_rdwr_val (addr, size);
   uint8_t a = __rt_rdwr_get_action (0, size);
   __rt_debug_rdwr (a, addr, size);
}


// memory loads
//
void __rt_load_pre   (const void *addr, uint32_t size)
{
   if (! do_load_store) return;
   uint8_t a = __rt_rdwr_get_action (1, size);
   __rt_log_action (a);
   __rt_log_addr (addr);
   __rt_check_oom (addr, size);
}

void __rt_load_post  (const void *addr, uint32_t size)
{
   if (! do_load_store) return;
   uint8_t a = __rt_rdwr_get_action (1, size);
   __rt_debug_rdwr (a, addr, size);
}


// memory management
//
// for heap allocation (malloc/free) this is done directly in mm.c
void __rt_allo (uint8_t *addr, uint32_t size)
{
   return; // FIXME - we currently disable this
   TRACE2 (RT_ALLOCA, addr, size);
}
void __rt_call (uint16_t id)
{
   return; // FIXME - we currently disable this
   TRACE3 (RT_CALL, id);
}
void __rt_ret (uint16_t id)
{
   return; // FIXME - we currently disable this
   TRACE3 (RT_RET, id);
}

void __rt_memreg_print (struct memreg *m, const char *prefix, const char *suffix)
{
   printf ("%s%16p - %16p, %4zu%s%s",
      prefix,
      m->begin,
      m->end,
      UNITS_SIZE (m->size),
      UNITS_UNIT (m->size),
      suffix);
}

void __rt_libc_init ()
{
   __rt_mm_init ();
   __rt_thread_init ();
   __rt_stdio_init ();
}

void __rt_libc_term ()
{
   __rt_thread_term ();
   __rt_mm_term ();
   __rt_stdio_term ();
}

int __rt_mainn (int argc, const char * const *argv, const char * const *env)
{
   int ret;
   unsigned i, n;
   const char * const *v;


   // assert that global const variables equal corresponding ones in the rt
   // structure
   ASSERT ((uint64_t) rt->mem.begin == memstart);
   ASSERT ((uint64_t) rt->mem.end == memend);
   ASSERT (rt->mem.size == (memend - memstart));
   ASSERT ((uint64_t) rt->trace.ev.end == evend); // xxxxxxxxxxxxxx
   ASSERT ((void *) rt->trace.evptr == rt->trace.ev.begin);
   ASSERT ((void *) rt->trace.addrptr == rt->trace.addr.begin);
   ASSERT ((void *) rt->trace.idptr == rt->trace.id.begin);
   ASSERT ((void *) rt->trace.valptr == rt->trace.val.begin);
   // at least one action in the replay
   ASSERT (rt->replay.size >= 1);
   // replay[0] is free mode or main thread
   ASSERT (rt->replay.tab[0].tid == -1 || rt->replay.tab[0].tid == 0);
   // last is a switch to free mode
   ASSERT (rt->replay.tab[rt->replay.size - 1].tid == -1);
   ASSERT (rt->replay.tab[rt->replay.size - 1].count == -1);
   // replay.current points to the beginning
   ASSERT (rt->replay.current == rt->replay.tab);
   for (i = 0; i < RT_MAX_THREADS; i++) ASSERT (rt->trace.num_blue[i] == 0);

   ASSERT (sizeof (float) == 4);
   ASSERT (sizeof (double) == 8);
   ASSERT (sizeof (long double) == 16);

   if (rt->flags.verbose)
   {
      // our little tribute to how all of this started... ;)
      PRINT ("stid: rt: main: I feel fantaastic... I feel the PUMP!");
      PRINT ("stid: rt: main: guest's address space:");
      __rt_memreg_print (&rt->mem, "stid: rt: main:  ", ", total guest memory\n");
      __rt_memreg_print (&rt->data, "stid: rt: main:  ", ", data (.data, .bss, .rodata, others)\n");
      __rt_memreg_print (&rt->heap, "stid: rt: main:  ", ", heap\n");
      __rt_memreg_print (&rt->t0stack, "stid: rt: main:  ", ", main stack\n");
      PRINT ("stid: rt: main: thread-local storage initializers:");
      __rt_memreg_print (&rt->tdata, "stid: rt: main:  ", ", .tdata/.tbss\n");
      PRINT ("stid: rt: main: event trace buffer:");
      __rt_memreg_print (&rt->trace.ev, "stid: rt: main:  ", ", event trace (8bit event ids)\n");
      __rt_memreg_print (&rt->trace.addr, "stid: rt: main:  ", ", event trace (64bit addr)\n");
      __rt_memreg_print (&rt->trace.val, "stid: rt: main:  ", ", event trace (64bit val)\n");
      __rt_memreg_print (&rt->trace.id, "stid: rt: main:  ", ", event trace (16bit call ids)\n");
      PRINT_ ("stid: rt: main: replay seq: ");
      for (i = 0; i < rt->replay.size; i++)
      {
         if (i) PRINT_ ("; ");
         if (rt->replay.tab[i].tid == -1)
         {
            PRINT ("-1");
         }
         else
         {
            // every context switch requests to replay at least 1 event
            ASSERT (rt->replay.tab[i].tid >= 0);
            ASSERT (rt->replay.tab[i].count >= 1);
            PRINT_ ("%u %u", rt->replay.tab[i].tid, rt->replay.tab[i].count);
         }
      }
      PRINT_ ("stid: rt: main: sleepset: ");
      for (i = 0; i < RT_MAX_THREADS; i++)
         if (rt->replay.sleepset[i])
            PRINT_ ("%d (%p); ", i, rt->replay.sleepset[i]);
      PRINT ("\nstid: rt: main: ======================");
   }

   // initialize subsystems (before this there is no guarantee that
   // std{in,out,err} are correctly initialized!!!
   __rt_libc_init ();

   // determine number of environment variables
   for (n = 0, v = env; *v; ++n, ++v)
   ;

   // copy arguments into our stack and heap
   char *myargv[argc];
   char *myenv[n+1];
   for (i = 0; i < (unsigned) argc; i++)
   {
      myargv[i] = __rt_malloc_uninitialized (strlen (argv[i]) + 1);
      strcpy (myargv[i], argv[i]);
   }
   for (i = 0; i < n; i++)
   {
      myenv[i] = __rt_malloc_uninitialized (strlen (env[i]) + 1);
      strcpy (myenv[i], env[i]);
   }
   myenv[n] = 0;

   if (rt->flags.verbose)
   {
      PRINT ("stid: rt: main: |argv| %d |env| %d", argc, n);
      for (i = 0; i < (unsigned) argc; i++)
         PRINT ("stid: rt: main: argv[%2i] %16p '%s'", i, myargv[i], myargv[i]);
      for (i = 0; i <= n; i++)
         PRINT ("stid: rt: main:  env[%2i] %16p '%s'", i, myenv[i], myenv[i]);
      __rt_debug_header ();
   }
   ret = main (argc, myargv, myenv);

   // do the instrumented verison of exit(3)
   _rt_exit (ret);

   // unreachable code
   return ret;
}

void __rt_cend (uint32_t exitcode)
{
   // termination functions for different subsystems
   __rt_libc_term ();

   // this function will be called from the exit prologue routine (__rt_end) and
   // will be the last thing executed before giving back control to the host
   rt->trace.size =
         (uint64_t) (rt->trace.evptr - (uint8_t*) rt->trace.ev.begin);

   //fflush (stdout);
   //fflush (stderr);

   __rt_end (exitcode);
}

void __rt_save_host_rsp (uint64_t rsp)
{
   rt->host_rsp = rsp;
}

uint64_t __rt_get_host_rsp ()
{
   return rt->host_rsp;
}

uint64_t __rt_get_memend ()
{
   return memend;
}

void __rt_panic ()
{
   ALERT ("panic, aborting!");
   abort ();
}

