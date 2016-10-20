
#ifndef __EXECUTOR_HH_
#define __EXECUTOR_HH_

#include <memory>

#include "llvm/IR/Module.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"

#include "../rt/rt.h"
#include "action_stream.hh"

#define ALIGN16(i) (((uint64_t) (i)) & 0xf ? (((uint64_t) (i)) + 16) & -16ul : (uint64_t) (i))

struct ExecutorConfig
{
   uint64_t memsize;
   uint64_t stacksize;
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
   void           set_replay (int *tab, int size);
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
