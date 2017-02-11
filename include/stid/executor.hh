
#ifndef __STID_EXECUTOR_HH_
#define __STID_EXECUTOR_HH_

#include <memory>


#include "llvm/IR/Module.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#undef DEBUG // will be (re)defined in ExecutionEngine.h
#include "llvm/ExecutionEngine/SectionMemoryManager.h"

#include "../../rt/rt.h"
#include "action_stream.hh"

#define ALIGN16(i) (((uint64_t) (i)) & 0xf ? (((uint64_t) (i)) + 16) & -16ul : (uint64_t) (i))

/// This structure holds information necessary to initialize the memory area
/// used to hold the guest code.
struct ExecutorConfig
{
   /// Size of the whole memory area used to hold the guest program, including
   /// the jitted code, data, bss, heap, and stacks of each thread.
   uint64_t memsize;
   /// Size of the stack of each thread. The stack of the main guest thread is
   /// allocated on the highest portion of the memory area allocated for the
   /// guest. The stack of every other thread is allocated by the runtime
   /// using malloc(3), they are thus in the guest's heap.
   uint64_t stacksize;
   /// Maximum number of actions that runtime will log during one execution of
   /// the guest.
   uint64_t tracesize;;
};

class Executor
{
public :
   std::vector<const char*> argv;
   std::vector<const char*> envp;
   int exitcode;

   Executor (std::unique_ptr<llvm::Module> mod, ExecutorConfig c);
   ~Executor ();

   void           run ();
   void           set_replay (const int *tab, int size);
   struct rt *    get_runtime ();
   action_streamt get_trace ();

private :
   struct rt                     rt;
   ExecutorConfig                conf;
   llvm::LLVMContext             &ctx;
   llvm::Module                  *m;
   llvm::ExecutionEngine         *ee;
   int replay_capacity;
   int (*entry) (int, const char* const*, const char* const*);

   void initialize_and_instrument_rt ();
   void instrument_events ();
   void jit_compile ();
   void optimize ();
   void restart_trace ();
   void detex_init ();
   void detex_apply ();
   void malloc_memreg (struct memreg *m, size_t size);
   llvm::Constant *ptr_to_llvm (void *ptr, llvm::Type *t);

   struct
   {
      unsigned rseed;
      std::vector<uint8_t> dataseg;
   } detex;
};

class MyMemoryManager : public llvm::SectionMemoryManager
{
public:
   MyMemoryManager (struct rt &rt) : rt (rt) {}

   uint8_t *allocateDataSection(uintptr_t Size, unsigned Alignment,
         unsigned SectionID, llvm::StringRef SectionName, bool isReadOnly)
         override;

private:
   struct rt &rt;
};

#endif
