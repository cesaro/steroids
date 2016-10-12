
#ifndef _RT_RT_H_
#define _RT_RT_H_

#include <inttypes.h>

#ifdef __cplusplus
extern "C" {
#endif

// these two are in start.s; host should invoke _rt_start
void _rt_start (int argc, const char * const *argv, const char * const *env);
void _rt_end (uint32_t exitcode);

// called from _rt_start, will call main()
int _rt_main (int argc, const char * const *argv, const char * const *env);

// used in start.s to access the "struct rt" in aha.c
void _rt_save_host_rsp (uint64_t rsp);
uint64_t _rt_get_host_rsp ();

// the user's main function, epic :)
int main (int argc, char **argv, char **env);

// instrumentation for loads
uint8_t  _rt_load8  (uint8_t  *addr);
uint16_t _rt_load16 (uint16_t *addr);
uint32_t _rt_load32 (uint32_t *addr);
uint64_t _rt_load64 (uint64_t *addr);
float    _rt_loadf  (float *addr);
double   _rt_loadd  (double *addr);
long double _rt_loadld (long double *addr);

// stores
void _rt_store8  (uint8_t  *addr, uint8_t  v);
void _rt_store16 (uint16_t *addr, uint16_t v);
void _rt_store32 (uint32_t *addr, uint32_t v);
void _rt_store64 (uint64_t *addr, uint64_t v);
void _rt_storef  (float *addr, float v);
void _rt_stored  (double *addr, double v);
void _rt_storeld (long double *addr, long double v);

// memory management
void _rt_allo (uint8_t *addr, uint32_t size);
void _rt_call (uint16_t id);
void _rt_ret  (uint16_t id);
void _rt_rllo (uint8_t *old, uint8_t *neww, uint64_t size);
void _rt_mllo (uint8_t *addr, uint64_t size); // malloc & calloc
void _rt_fre  (uint8_t *addr);

// others
void _rt_sig  (uint32_t signal);
void _rt_ctsw (uint32_t id);

// mutex lock and unlock - FIXME
void _rt_pthread_create(uint32_t id);
void _rt_pthread_join (uint32_t id);
void _rt_pthread_mutex_init (uint8_t *addr, uint32_t val); // mutex type
void _rt_pthread_mutex_lock (uint8_t *addr);
void _rt_pthread_mutex_unlock (uint8_t *addr);
void _rt_pthread_exit ();

// heap management routines
void *_rt_malloc  (size_t size);
void  _rt_free    (void *ptr);
void *_rt_realloc (void *ptr, size_t size);

void _rt_panic ();

enum eventtype
{
   // loads
   _RD8,
   _RD16,
   _RD32,
   _RD64,
   _RD128,
   // stores
   _WR8,
   _WR16,
   _WR32,
   _WR64,
   _WR128,
   // memory management
   _ALLO,
   _MLLO,
   _FREE,
   _CALL,
   _RET,
   // threads
   _THCREAT,
   _THJOIN,
   _THEXIT,
   _THSW,
   // locks
   _MTXINIT,
   _MTXLOCK,
   _MTXUNLK,
   // misc
   _NONE, // this should be the last in the list
};

struct memreg
{
   uint8_t *begin;
   uint8_t *end;
   size_t size;
};

struct eventrace {
   struct memreg ev;
   struct memreg addr;
   struct memreg val;
   struct memreg id;

   uint8_t  *evptr;
   uint64_t *addrptr;
   uint64_t *valptr;
   uint16_t *idptr;
   
   uint64_t size;
};

struct rt
{
   // contains the entire memory region allocated for the guest
   struct memreg mem;

   // subregions inside of "mem"
   struct memreg data;
   struct memreg heap;
   struct memreg stacks;

   // event trace
   struct eventrace trace;

   // stack pointer of the host upon entry on guest code
   uint64_t host_rsp;
};

// const for fast address checking without memory access, defined in rt.c
// static const uint64_t memstart;
// static const uint64_t memend;
// static const uint64_t evend;
// static struct rt * const rt;

static inline const char *_rt_ev_to_str (enum eventtype e)
{
   switch (e)
   {
   // loads
   case _RD8       : return "RD8    ";
   case _RD16      : return "RD16   ";
   case _RD32      : return "RD32   ";
   case _RD64      : return "RD64   ";
   case _RD128     : return "RD128  ";
   // stores
   case _WR8       : return "WR8    ";
   case _WR16      : return "WR16   ";
   case _WR32      : return "WR32   ";
   case _WR64      : return "WR64   ";
   case _WR128     : return "WR128  ";
   // memory management
   case _ALLO      : return "ALLO   ";
   case _MLLO      : return "MLLO   ";
   case _FREE      : return "FREE   ";
   case _CALL      : return "CALL   ";
   case _RET       : return "RET    ";
   // threads
   case _THCREAT   : return "THCREAT";
   case _THJOIN    : return "THJOIN ";
   case _THEXIT    : return "THEXIT ";
   case _THSW      : return "THSW   ";
   // locks
   case _MTXINIT   : return "MTXINIT";
   case _MTXLOCK   : return "MTXLOCK";
   case _MTXUNLK   : return "MTXUNLK";
   // misc
   case _NONE      : return "NONE";
   }
}


#ifdef __cplusplus
} // extern "C"
#endif

#endif // _RT_RT_H_

