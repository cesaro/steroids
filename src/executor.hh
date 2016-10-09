
#ifndef __EXECUTOR_HH_
#define __EXECUTOR_HH_

#include <memory>

#include "llvm/IR/Module.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"

#include "../rt/rt.h"

struct ExecutorConfig
{
   uint64_t memsize;
   uint64_t stacksize;
   uint64_t tracesize;;
};

class Executor
{
public :
   std::vector<const char *> argv;
   std::vector<const char *> envp;
   int exitcode;

   Executor (std::unique_ptr<llvm::Module> mod, ExecutorConfig c);

   ~Executor ();

   void initialize ();
   void run ();

private :
   bool                          initialized;
   struct rt                     rt;
   ExecutorConfig                conf;
   llvm::LLVMContext             &ctx;
   llvm::Module                  *m;
   llvm::ExecutionEngine         *ee;

   llvm::Constant *ptr_to_llvm (void *ptr, llvm::Type *t);
   void initialize_and_instrument_rt ();
   void instrument_events ();
};

#endif
