

// this file will be #include'd in the main.c file

static const char *_rt_quote (uint64_t v)
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

static void _rt_debug_header ()
{
   printf ("stid: rt: what                  addr              value comments\n");
   printf ("stid: rt: ======= ================== ================== ======================\n");
}

static void _rt_debug_trace0 (enum eventtype e)
{
   printf ("stid: rt: %s\n", _rt_ev_to_str (e));
}

static void _rt_debug_trace1 (enum eventtype e, const void *addr)
{
   printf ("stid: rt: %s %18p\n",
         _rt_ev_to_str (e), addr);
}

static void _rt_debug_trace2 (enum eventtype e, const void *addr, uint64_t v)
{
   printf ("stid: rt: %s %18p %#18lx %s\n",
         _rt_ev_to_str (e), addr, v, _rt_quote (v));
}

static void _rt_debug_trace3 (enum eventtype e, uint16_t v)
{
   printf ("stid: rt: %s                    %#18x %s\n",
         _rt_ev_to_str (e), v, _rt_quote (v));
}
static void _rt_debug_trace128 (enum eventtype e, const void *addr, long double v)
{
   
   printf ("stid: rt: %s %18p       [2 x 64bits] %.20Le\n",
         _rt_ev_to_str (e), addr, v);
}

static inline void _rt_check_limits_addr (const void *ptr, enum eventtype e)
{
   // if the memory access is offlimits, we finish execution
   if ((uint64_t) ptr < memstart || (uint64_t) ptr >= memend)
   {
      TRACE1 (e, ptr); // temporary solution to see RD OOMs
      printf ("stid: rt: out-of-memory access: event %d addr %p\n", e, ptr);
      _rt_end (255);
   }
}

static inline void _rt_check_trace_capacity ()
{
   // if the buffer space is exhausted, we finish
   // observe that this requires only one memory access (for evptr)
   if ((uint64_t) rt->trace.evptr == evend) _rt_end (255);
}

// memory loads - FIXME, later on we should make RD into TRACE1
//
uint8_t  _rt_load8  (uint8_t  *addr)
{
   uint8_t v;
   _rt_check_limits_addr ((void*) addr, _RD8);
   v = *addr;
   TRACE2 (_RD8, addr, v);
   _rt_check_trace_capacity ();
   return v;
}
uint16_t _rt_load16 (uint16_t *addr)
{
   uint16_t v;
   _rt_check_limits_addr ((void*) addr, _RD16);
   v = *addr;
   TRACE2 (_RD16, addr, v);
   _rt_check_trace_capacity ();
   return v;
}
uint32_t _rt_load32 (uint32_t *addr)
{
   uint32_t v;
   _rt_check_limits_addr ((void*) addr, _RD32);
   v = *addr;
   TRACE2 (_RD32, addr, v);
   _rt_check_trace_capacity ();
   return v;
}
uint64_t _rt_load64 (uint64_t *addr)
{
   uint64_t v;
   _rt_check_limits_addr ((void*) addr, _RD64);
   v = *addr;
   TRACE2 (_RD64, addr, v);
   _rt_check_trace_capacity ();
   return v;
}
float    _rt_loadf  (float *addr)
{
   float v;
   ASSERT (sizeof (float) == 4)
   _rt_check_limits_addr ((void*) addr, _RD32);
   v = *addr;
   TRACE2 (_RD32, addr, * (uint32_t*) (void*) &v);
   _rt_check_trace_capacity ();
   return v;
}
double   _rt_loadd  (double *addr)
{
   double v;
   ASSERT (sizeof (double) == 8)
   _rt_check_limits_addr ((void*) addr, _RD64);
   v = *addr;
   TRACE2 (_RD64, addr, * (uint64_t*) (void*) &v);
   _rt_check_trace_capacity ();
   return v;
}
long double _rt_loadld (long double *addr)
{
   long double v;
   ASSERT (sizeof (long double) == 16)
   _rt_check_limits_addr ((void*) addr, _RD128);
   v = *addr;
   TRACE128 (_RD128, addr, v); // event, address, 2 x val
   _rt_check_trace_capacity ();
   return v;
}

// memory stores
//
void _rt_store8 (uint8_t *addr, uint8_t v)
{
   TRACE2 (_WR8, addr, v);
   _rt_check_limits_addr ((void*) addr, _WR8);
   _rt_check_trace_capacity ();
}
void _rt_store16 (uint16_t *addr, uint16_t v)
{
   TRACE2 (_WR16, addr, v);
   _rt_check_limits_addr ((void*) addr, _WR16);
   _rt_check_trace_capacity ();
}
void _rt_store32 (uint32_t *addr, uint32_t v)
{
   TRACE2 (_WR32, addr, v);
   _rt_check_limits_addr ((void*) addr, _WR32);
   _rt_check_trace_capacity ();
}
void _rt_store64 (uint64_t *addr, uint64_t v)
{
   TRACE2 (_WR64, addr, v);
   _rt_check_limits_addr ((void*) addr, _WR64);
   _rt_check_trace_capacity ();
}
void _rt_storef  (float *addr, float v)
{
   uint32_t vv = * ((uint32_t*) (void*) &v);
   TRACE2 (_WR32, addr, vv);
   _rt_check_limits_addr ((void*) addr, _WR32);
   _rt_check_trace_capacity ();
}
void _rt_stored  (double *addr, double v)
{
   uint64_t vv = * ((uint64_t*) (void*) &v);
   TRACE2 (_WR64, addr, vv);
   _rt_check_limits_addr ((void*) addr, _WR64);
   _rt_check_trace_capacity ();
}
void _rt_storeld (long double *addr, long double v)
{
   TRACE128 (_WR128, addr, v); // event, address, 2 x val
   _rt_check_limits_addr ((void*) addr, _WR128);
   _rt_check_trace_capacity ();
}

// memory management
//
// for heap allocation (malloc/free) this is done directly in mm.c
void _rt_allo (uint8_t *addr, uint32_t size)
{
   TRACE2 (_ALLO, addr, size);
}
void _rt_call (uint16_t id)
{
   TRACE3 (_CALL, id);
}
void _rt_ret (uint16_t id)
{
   TRACE3 (_RET, id);
}
void _rt_exit ()
{
   TRACE0 (_THEXIT);
}

void _rt_memreg_print (struct memreg *m, const char *prefix, const char *suffix)
{
   printf ("%s%p - %p, %4zu%s%s",
      prefix,
      m->begin,
      m->end,
      UNITS_SIZE (m->size),
      UNITS_UNIT (m->size),
      suffix);
}

int _rt_main (int argc, const char * const *argv, const char * const *env)
{
   int ret;
	int i, n;
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

   // assert here that we did correct assumptions in _rt_{load,store}{f,d,ld}
   ASSERT (sizeof (float) == 4)
   ASSERT (sizeof (double) == 8)
   ASSERT (sizeof (long double) == 16)

   printf ("stid: rt: main: I feel fantastic... I feel the PUMP!\n");
   printf ("stid: rt: main: guest's address space:\n");
   _rt_memreg_print (&rt->mem, "stid: rt: main:  ", ", total guest memory\n");
   _rt_memreg_print (&rt->data, "stid: rt: main:  ", ", data (.data, .bss, .rodata, and others)\n");
   _rt_memreg_print (&rt->heap, "stid: rt: main:  ", ", heap\n");
   _rt_memreg_print (&rt->stacks, "stid: rt: main:  ", ", stacks\n");
   printf ("stid: rt: main: event trace buffer:\n");
   _rt_memreg_print (&rt->trace.ev, "stid: rt: main:  ", ", event trace (8bit event ids)\n");
   _rt_memreg_print (&rt->trace.addr, "stid: rt: main:  ", ", event trace (64bit addr)\n");
   _rt_memreg_print (&rt->trace.val, "stid: rt: main:  ", ", event trace (64bit val)\n");
   _rt_memreg_print (&rt->trace.id, "stid: rt: main:  ", ", event trace (16bit call ids)\n");
   printf ("stid: rt: main: ======================\n");

   // initialize the heap
   _rt_mm_init ();

	// determine number of environment variables
	for (n = 0, v = env; *v; ++n, ++v)
	;

   // copy arguments into our stack and heap
   char *myargv[argc];
   char *myenv[n+1];
   for (i = 0; i < argc; i++)
   {
      myargv[i] = malloc (strlen (argv[i]) + 1);
      strcpy (myargv[i], argv[i]);
   }
   for (i = 0; i < n; i++)
   {
      myenv[i] = malloc (strlen (env[i]) + 1);
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

   fflush (stdout);
   fflush (stderr);

   // EXIT event for the main thread - should this be called from _rt_end?
   TRACE0 (_THEXIT);
   rt->trace.size = (uint64_t) (rt->trace.evptr - (uint8_t*) rt->trace.ev.begin);

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


