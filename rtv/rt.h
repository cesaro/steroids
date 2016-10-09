
#ifndef _RT_RT_H_
#define _RT_RT_H_

#include <inttypes.h>

#ifdef __cplusplus
extern "C" {
#endif

// these two are in start.s; host should invoke _rt_start
void _rt_start (int argc, const char * const *argv, const char * const *env);
void _rt_end (void);

// called from _rt_start, will call main()
int _rt_main (int argc, const char * const *argv, const char * const *env);

// the user's main function, epic :)
int main (int argc, char **argv, char **env);

// instrumentation for loads
void _rt_load8  (uint8_t  *addr, uint8_t  v);
void _rt_load16 (uint16_t *addr, uint16_t v);
void _rt_load32 (uint32_t *addr, uint32_t v);
void _rt_load64 (uint64_t *addr, uint64_t v);

// stores
void _rt_store8  (uint8_t  *addr, uint8_t  v);
void _rt_store16 (uint16_t *addr, uint16_t v);
void _rt_store32 (uint32_t *addr, uint32_t v);
void _rt_store64 (uint64_t *addr, uint64_t v);

// memory management
void _rt_allo (uint8_t *addr, uint32_t size);
void _rt_mllo (uint8_t *addr, uint64_t size); // malloc & calloc
void _rt_rllo (uint8_t *old, uint8_t *neww, uint64_t size);
void _rt_fre  (uint8_t *addr);
void _rt_call (uint32_t id);
void _rt_ret  (uint32_t id);

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
   NOP = 0,
   // loads
   RD8,
   RD16,
   RD32,
   RD64,
   // stores
   WR8,
   WR16,
   WR32,
   WR64,
   // memory allocation
   ALLO,
   MLLO,
   FREE,
   CALL,
   RET,
   // threads
   THCREAT,
   THJOIN,
   THSW,
   // locks
   MTXINIT,
   MTXLOCK,
   MTXUNCLOK,
   // misc
   EVFULL,
};

struct rt
{
	uint64_t memstart;
	uint64_t memend;
	uint64_t memsize;

	uint64_t stackstart;
	uint64_t stackend;
	uint64_t stacksize;

   struct {
	   uint8_t  *evstart;
	   uint8_t  *evend;
	   uint8_t  *evptr;
      uint64_t *addrstart;
	   uint64_t *addrptr;
      uint16_t *idstart;
      uint16_t *idptr;
      uint64_t *valstart; // optional
	   uint64_t *valptr;
   } trace;

	uint64_t host_rsp;
};

// const for fast address checking without memory access, defined in rt.c
// static const uint64_t memstart;
// static const uint64_t memend;
// static const uint64_t evend;
// static struct rt * const rt;

#ifdef __cplusplus
} // extern "C"
#endif

#endif // _RT_RT_H_
