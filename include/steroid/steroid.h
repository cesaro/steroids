
#ifndef _STEROID_STEROID_H_
#define _STEROID_STEROID_H_

#ifdef __cplusplus
extern "C" {
#endif

#include <steroid/util/da.h>
#include <stdint.h>

// memory operations
#define STID_WR         0
#define STID_RD         1

// thread syncrhonization
#define STID_LOCK       2
#define STID_UNLOCK     3
#define STID_CREATE     4
#define STID_JOIN       5
#define STID_CTSW       6
#define STID_EXIT       7

// memory management
#define STID_MALLOC     8
#define STID_FREE       9
#define STID_ALLOCA     10
#define STID_CALL       11
#define STID_RET        12

/* error codes
 * - 0 return code : ok
 * - non-zero return code : error
 */

// opaque
struct stid;

struct stid_ctsw
{
   int thid;
   int nrev; 
};

struct stid_replay
{
   struct da tab; // array of stid_ctsw
};

struct stid_action
{
   int      type;
   size_t   addr;
   uint64_t val;
};

struct stid_exec
{
   struct da tab; // array of stid_action's
};

struct stid_event
{
   struct stid_action a;
   struct
   {
      unsigned th;
      unsigned other; // ideally the stream position
   } idx;
   struct stid_event *pre_proc;
   struct stid_event *pre_mem;
};

struct stid_po
{
   struct da max_proc; // array of stid_event
   struct da max_lock; // array of stid_event
};

// constructor and destructor
struct stid * stid_init ();
int stid_term (struct stid *s);

// load an LLVM bytecode file
int stid_load_bytecode (struct stid *s, const char *path);

// run the JIT compiled code and generate the action stream
int stid_run (struct stid *s, struct stid_replay *rep);

// get the (public version of) the action stream
int stid_get_seqexec (struct stid *s, struct stid_exec *run);

// get the (public version of) the lock partial order
int stid_get_poexec (struct stid *s, struct stid_po *po);

int stid_test ();

// testing steroid actions
struct stid_action * stid_get_action();

int stid_print_action(struct stid_action *act);

// testing steroid context switches
struct stid_ctsw * stid_get_ctsw();

int stid_print_ctsw(struct stid_ctsw *ctx);

// testing steroid replay
struct stid_replay * stid_get_replay();

int stid_check_replay(struct stid_replay *rep);

#ifdef __cplusplus
} // extern "C"
#endif

#endif // _STEROID_STEROID_H_
