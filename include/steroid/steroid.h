
#ifndef _STEROID_STEROID_H_
#define _STEROID_STEROID_H_

// memory operations
#define STEROID_WR         0
#define STEROID_RD         1

// thread syncrhonization
#define STEROID_LOCK       2
#define STEROID_UNLOCK     3
#define STEROID_CREATE     4
#define STEROID_JOIN       5
#define STEROID_CTSW       6
#define STEROID_EXIT       7

// memory management
#define STEROID_MALLOC     8
#define STEROID_FREE       9
#define STEROID_ALLOCA     10
#define STEROID_CALL       11
#define STEROID_RET        12

#include <steroid/util/da.h>

/* error codes
 * - 0 return code : ok
 * - non-zero return code : error
 */

// opaque
struct steroid;

struct steroid_ctsw
{
   unsigned thid;
   unsigned nrev; 
};

struct steroid_replay
{
   struct da tab;
};

struct steroid_action
{
   int      type;
   size_t   addr;
   uint64_t val;
};

struct steroid_exec
{
   struct da tab; // array of steroid_action's
};

struct steroid_event
{
   struct steroid_action a;
   struct
   {
      unsigned th;
      unsigned other; // ideally the stream position
   } idx;
   struct steroid_event *pre_proc;
   struct steroid_event *pre_mem;
};

struct steroid_po
{
   struct da max_proc;
   struct da max_lock;
};


// constructor and destructor
struct steroid * steroid_init ():
int steroid_term (struct steroid *s);


// load an LLVM bytecode file
int steroid_load_bytecode (struct steroid *s, const char *path);

// run the JIT compiled code and generate the action stream
int steroid_run (struct steroid *s, struct steroid_replay *rep);

// get the (public version of) the action stream
int steroid_get_seqexec (struct steroid *s, struct steroid_exec *run);

// get the (public version of) the lock partial order
int steroid_get_poexec (struct steroid *s, struct steroid_po *po);

#endif
