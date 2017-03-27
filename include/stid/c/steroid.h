
#ifndef _STEROID_STEROID_H_
#define _STEROID_STEROID_H_

#ifdef __cplusplus
extern "C" {
#endif

#include <stid/c/util/da.h>
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
#define STID_ENTRY      8

// memory management
#define STID_MALLOC     9
#define STID_FREE       10
#define STID_ALLOCA     11
#define STID_CALL       12
#define STID_RET        13

// see code
#define STID_CMD1       0

/* error codes
 * - 0 return code : ok
 * - non-zero return code : error
 */

// opaque
struct stid_handle;

struct stid_ctsw
{
   unsigned int thid;
   unsigned int nrev;
};

struct stid_replay
{
   struct da tab; // array of stid_ctsw
};

struct stid_action
{
   int      type;
   uint64_t addr;
   uint64_t val;
};

struct stid_exec
{
   struct da tab; // array of stid_action's
};

struct stid_event
{
   struct stid_action act;
   struct
   {
      unsigned int tid;
      unsigned int idx;
   } pre_mem;
   unsigned int sidx; // the index in the stream
};

struct stid_po
{
   struct da procs;     // array of (pointers to) arrays of stid_events
   struct da max_lock;  // array of stid_event (one per lock ever used in the execution)
};

// constructor and destructor
struct stid_handle * stid_init ();
int stid_term (struct stid_handle *s);

// load an LLVM bytecode file
int stid_load_bytecode (struct stid_handle *s, const char *path);

// adding arguments to the commandline of the program
void stid_argv_add (struct stid_handle *s, const char *arg);
void stid_argv_clear (struct stid_handle *s);

// run the JIT compiled code and generate the action stream
int stid_run (struct stid_handle *s, struct stid_replay *rep);

// get the (public version of) the action stream
int stid_get_seqexec (struct stid_handle *s, struct stid_exec *run);

// operations on the po execution:
struct stid_po *stid_po_get (struct stid_handle *s);
int             stid_po_term (struct stid_po *po);
int             stid_po_print (const struct stid_po *po);

int stid_event_print (const struct stid_event *e, unsigned tid, unsigned idx);
int stid_action_print (struct stid_action *act);
const char *stid_action_type2str (int a);

// testing
int stid_test ();
int stid_test_checker ();

// arbitrary commands to the internals of the steroids engine
int stid_cmd (struct stid_handle *s, int cmd, void *arg1, void *arg2, void *arg3);

#ifdef __cplusplus
} // extern "C"
#endif

#endif // _STEROID_STEROID_H_
