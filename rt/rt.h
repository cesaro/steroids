
#ifndef _RT_RT_H_
#define _RT_RT_H_

#include <inttypes.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

// configuration
#define RT_MAX_THREADS 128
#define RT_DEFAULT_STACK_SIZE (8 << 20) // 8M
#define RT_MAX_OWNED_MUTEXES 64
#define RT_MAX_ATEXIT_FUNS 64

// event identifiers:
// - event class: upper 3 bits
// - event id:    lower 5 bits
#define RT_ACTION_CLASS_MASK  0xe0  // 1110 0000
#define RT_ACTION_ID_MASK     0x1f  // 0001 1111

// event classes
#define RT_ACTION_CLASS_RD8   0x00  // 000 0
#define RT_ACTION_CLASS_RD64  0x20  // 001 0
#define RT_ACTION_CLASS_WR8   0x40  // 010 0
#define RT_ACTION_CLASS_WR64  0x60  // 011 0
#define RT_ACTION_CLASS_MM    0x80  // 100 0
#define RT_ACTION_CLASS_TH    0xa0  // 101 0

// event ids: RD8 : number of bytes read, from 1 to 4
// event ids: RD64: number of 64bit words read, from 1 to 31
// event ids: WR8/WR64: idem
// event ids: MM:
#define RT_ACTION_ALLOCA      0x01  // 0 0001
#define RT_ACTION_MALLOC      0x02  // 0 0010
#define RT_ACTION_FREE        0x03  // 0 0011
#define RT_ACTION_CALL        0x04  // 0 0100
#define RT_ACTION_RET         0x05  // 0 0101
// event ids: TH
#define RT_ACTION_THCREAT     0x00  // 0 0000
#define RT_ACTION_THJOIN      0x01  // 0 0001
#define RT_ACTION_THEXIT      0x02  // 0 0010
#define RT_ACTION_THCTXSW     0x03  // 0 0011
#define RT_ACTION_MTXLOCK     0x04  // 0 0100
#define RT_ACTION_MTXUNLK     0x05  // 0 0101

// get the action class or the event id
#define RT_ACTION_CLASS(x)    ((x) & RT_ACTION_CLASS_MASK)
#define RT_ACTION_ID(x)       ((x) & RT_ACTION_ID_MASK)

// often used event ids
#define RT_RD8                (RT_ACTION_CLASS_RD8 | 1)
#define RT_RD16               (RT_ACTION_CLASS_RD8 | 2)
#define RT_RD32               (RT_ACTION_CLASS_RD8 | 4)
#define RT_RD64               (RT_ACTION_CLASS_RD64 | 1)
#define RT_RD128              (RT_ACTION_CLASS_RD64 | 2)
#define RT_RD192              (RT_ACTION_CLASS_RD64 | 3)
#define RT_RD256              (RT_ACTION_CLASS_RD64 | 4)
#define RT_WR8                (RT_ACTION_CLASS_WR8 | 1)
#define RT_WR16               (RT_ACTION_CLASS_WR8 | 2)
#define RT_WR32               (RT_ACTION_CLASS_WR8 | 4)
#define RT_WR64               (RT_ACTION_CLASS_WR64 | 1)
#define RT_WR128              (RT_ACTION_CLASS_WR64 | 2)
#define RT_WR192              (RT_ACTION_CLASS_WR64 | 3)
#define RT_WR256              (RT_ACTION_CLASS_WR64 | 4)
#define RT_ALLOCA             (RT_ACTION_CLASS_MM | RT_ACTION_ALLOCA)
#define RT_MALLOC             (RT_ACTION_CLASS_MM | RT_ACTION_MALLOC)
#define RT_FREE               (RT_ACTION_CLASS_MM | RT_ACTION_FREE)
#define RT_CALL               (RT_ACTION_CLASS_MM | RT_ACTION_CALL)
#define RT_RET                (RT_ACTION_CLASS_MM | RT_ACTION_RET)
#define RT_THCREAT            (RT_ACTION_CLASS_TH | RT_ACTION_THCREAT)
#define RT_THJOIN             (RT_ACTION_CLASS_TH | RT_ACTION_THJOIN)
#define RT_THEXIT             (RT_ACTION_CLASS_TH | RT_ACTION_THEXIT)
#define RT_THCTXSW            (RT_ACTION_CLASS_TH | RT_ACTION_THCTXSW)
#define RT_MTXLOCK            (RT_ACTION_CLASS_TH | RT_ACTION_MTXLOCK)
#define RT_MTXUNLK            (RT_ACTION_CLASS_TH | RT_ACTION_MTXUNLK)

#define RT_IS_MULTIW_RDWR(x)  (RT_IS_MULTIW_RD(x) || RT_IS_MULTIW_WR(x))
#define RT_IS_MULTIW_RD(x)    (((x) & RT_ACTION_CLASS_MASK) == RT_ACTION_CLASS_RD64)
#define RT_IS_MULTIW_WR(x)    (((x) & RT_ACTION_CLASS_MASK) == RT_ACTION_CLASS_WR64)
#define RT_MULTIW_COUNT(x)    ((x) & RT_ACTION_ID_MASK)

// these two are in start.s; host should invoke __rt_start
void __rt_start (int argc, const char * const *argv, const char * const *env);
void __rt_end (uint32_t exitcode);

// called from __rt_start, will call main()
int __rt_mainn (int argc, const char * const *argv, const char * const *env);

// call this function to exit, with errors or normal exit
void __rt_cend (uint32_t exitcode);

// new instrumentation for load and stores
void __rt_store_pre  (const void *addr, uint32_t size);
void __rt_store_post (const void *addr, uint32_t size);
void __rt_load_pre   (const void *addr, uint32_t size);
void __rt_load_post  (const void *addr, uint32_t size);

// memory management
void __rt_allo (uint8_t *addr, uint32_t size);
void __rt_call (uint16_t id);
void __rt_ret  (uint16_t id);
void __rt_fre  (uint8_t *addr);

// used in start.s to access the "struct rt" in main.c
void __rt_save_host_rsp (uint64_t rsp);
uint64_t __rt_get_host_rsp ();

// stop execution, print an error and enter an infinite loop
void __rt_panic ();


// libc (internal)
void __rt_libc_init ();
void __rt_libc_term ();

// stdlib.h
void __rt_mm_init (); // internal
void __rt_mm_term (); // internal
void *_rt_malloc_uninitialized (size_t size); // internal
void *_rt_malloc  (size_t size);
void  _rt_free    (void *ptr);
void *_rt_realloc (void *ptr, size_t size);
int   _rt_atexit (void (* fun) (void));
void  _rt_exit (int status);
void _rt_abort ();

// unistd.h
void  _rt__exit (int status);
unsigned int _rt_sleep (unsigned int secs);
int   _rt_usleep (useconds_t us);

// errno.h
int *_rt___errno_location ();

// pthread.h -> see rt/pthread.h

// memory region
struct memreg
{
   uint8_t *begin;
   uint8_t *end;
   size_t size;
};

// stores the stream of actions that our dynamic analysis is interested in
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

   int    num_ths;
   size_t num_blue[RT_MAX_THREADS];
};

struct reptrace
{
   // points to an array of integers [tid1,nr,tid2,nr,tid3,nr...,-1]
   int *tab;
   // size of the array above
   int size;
   // pointer to the current event count in the array above
   int *current;
};

// this stores the entire state of the runtime
struct rt
{
   // describes the entire memory region allocated for the guest
   struct memreg mem;

   // subregions inside of "mem"
   struct memreg data;
   struct memreg heap;
   struct memreg stacks;

   // event trace
   struct eventrace trace;
   struct reptrace replay;

   // stack pointer of the host upon entry on guest code
   uint64_t host_rsp;
};

// const for fast address checking without memory access, defined in main.c
// static const uint64_t memstart;
// static const uint64_t memend;
// static const uint64_t evend;
// static struct rt * const rt;

static inline const char *__rt_action_to_str (uint8_t a)
{
   static char str[20];

   switch (a)
   {
   // most common loads
   case RT_RD8     : return "RD8    ";
   case RT_RD16    : return "RD16   ";
   case RT_RD32    : return "RD32   ";
   case RT_RD64    : return "RD64   ";
   case RT_RD128   : return "RD128  ";
   case RT_RD192   : return "RD192  ";
   case RT_RD256   : return "RD256  ";
   // most common stores
   case RT_WR8     : return "WR8    ";
   case RT_WR16    : return "WR16   ";
   case RT_WR32    : return "WR32   ";
   case RT_WR64    : return "WR64   ";
   case RT_WR128   : return "WR128  ";
   case RT_WR192   : return "WR192  ";
   case RT_WR256   : return "WR256  ";
   // memory management
   case RT_ALLOCA  : return "ALLO   ";
   case RT_MALLOC  : return "MLLO   ";
   case RT_FREE    : return "FREE   ";
   case RT_CALL    : return "CALL   ";
   case RT_RET     : return "RET    ";
   // threads
   case RT_THCREAT : return "THCREAT";
   case RT_THJOIN  : return "THJOIN ";
   case RT_THEXIT  : return "THEXIT ";
   case RT_THCTXSW : return "THCTXSW";
   // locks
   case RT_MTXLOCK : return "MTXLOCK";
   case RT_MTXUNLK : return "MTXUNLK";

   // less common loads / stores
   default :
      if (RT_IS_MULTIW_RD (a))
      {
         // min 5 max 31
         sprintf (str, "RD [%u x i64]", RT_MULTIW_COUNT (a));
         return str;
      }
      if (RT_IS_MULTIW_WR (a))
      {
         // min 5 max 31
         sprintf (str, "WR [%u x i64]", RT_MULTIW_COUNT (a));
         return str;
      }
      return "???    ";
   }
}


#ifdef __cplusplus
} // extern "C"
#endif

#endif // _RT_RT_H_

