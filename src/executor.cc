
#include <vector>
#include <utility>
#include <stdlib.h>

#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/ExecutionEngine/MCJIT.h"

#undef DEBUG // exported by ExecutionEngine.h
#include "verbosity.h"
#include "misc.hh"
#include "../rt/rt.h"
#include "instrumenter.hh"
#include "executor.hh"

uint8_t *MyMemoryManager::allocateDataSection(uintptr_t Size, unsigned Alignment,
         unsigned SectionID, llvm::StringRef SectionName, bool isReadOnly)
{
   uint8_t *ptr;

   ptr = (uint8_t *) ALIGN16 ((uint64_t) rt.data.end);
   rt.data.end = ptr + Size;
   rt.data.size = rt.data.end - rt.data.begin;

   //ptr = llvm::SectionMemoryManager::allocateDataSection (Size, Alignment, SectionID, SectionName, isReadOnly);

   DEBUG ("stid: executor: memory manager: allocation request: ptr %16p size %6zd align %d secid %d ro %d secname '%s'",
         ptr,
         Size, Alignment, SectionID, isReadOnly, SectionName.str().c_str());
   return ptr;
}

Executor::Executor (std::unique_ptr<llvm::Module> mod, ExecutorConfig c) :
   conf (c),
   ctx (mod->getContext ()),
   m (mod.get()),
   ee (0)
{
   std::string errors;
   llvm::EngineBuilder eb (std::move(mod));

   // make sure we can safely destroy the object
   rt.mem.begin = 0;
   rt.trace.ev.begin = 0;
   rt.trace.addr.begin = 0;
   rt.trace.id.begin = 0;
   rt.trace.val.begin = 0;
   rt.trace.num_ths = 1;
   rt.trace.num_mutex = 0;

   // create a memory manager for the JIT engine (the builder owns the object)
   std::unique_ptr<llvm::RTDyldMemoryManager> mm (new MyMemoryManager (rt));

   // create a JIT execution engine, using the (new) MCJIT engine
   eb.setErrorStr (&errors);
   eb.setOptLevel (llvm::CodeGenOpt::Level::Aggressive);
   eb.setVerifyModules (true);
   //eb.setMCJITMemoryManager(llvm::make_unique<llvm::SectionMemoryManager>());
   eb.setMCJITMemoryManager(std::move (mm));
   ee = eb.create();
   if (! ee)
   {
      std::string s = fmt ("Executor: unable to create ExecutionEngine: %s", errors.c_str());
      throw std::runtime_error (s);
   }

   // tell the module the way the target lays out data structures
   m->setDataLayout (*ee->getDataLayout());

   // initialize guest memory area and instrument the llvm module
   initialize_and_instrument_rt ();
   instrument_events ();
}

Executor::~Executor ()
{
   // delete first the execution engine, which will still do things in the JITed
   // code held in guest memory
   delete ee;
   free (rt.mem.begin);
   free (rt.trace.ev.begin);
   free (rt.trace.addr.begin);
   free (rt.trace.id.begin);
   free (rt.trace.val.begin);
}

void Executor::malloc_memreg (struct memreg *m, size_t size)
{
   m->size = size;
   m->begin = (uint8_t *) malloc (size);
   m->end = m->begin + m->size;
}

void Executor::initialize_and_instrument_rt ()
{
   DEBUG ("stid: executor: allocating guest memory");
   // allocate the memory space for the guest code (heap + stacks)
   malloc_memreg (&rt.mem, conf.memsize);
   if (rt.mem.begin == 0)
      throw std::runtime_error ("malloc: cannot prepare memory for code execution");

   // the stacks should fit into the main memory
   ASSERT (conf.stacksize < conf.memsize);
   // stacks are located at the end of the memory
   rt.stacks.size = conf.stacksize;
   rt.stacks.end = rt.mem.end;
   rt.stacks.begin = rt.stacks.end - rt.stacks.size;

   // the data segment will be initialized when the JIT engine calls our
   // MyMemoryManager, which in turn grows this section to place the .data,
   // .bss, .rodata, etc. sections here
   rt.data.begin = rt.mem.begin;
   rt.data.end = rt.mem.begin;
   rt.data.size = 0;

   // allocate memory for the event stream, ids (uint8_t)
   ASSERT (_NONE < 256);
   malloc_memreg (&rt.trace.ev, conf.tracesize);
   rt.trace.evptr = rt.trace.ev.begin;
   if (rt.trace.ev.begin == 0)
      throw std::runtime_error ("malloc: cannot allocate memory for log trace");

   // addr operands
   malloc_memreg (&rt.trace.addr, conf.tracesize * sizeof (uint64_t));
   rt.trace.addrptr = (uint64_t *) rt.trace.addr.begin;
   if (rt.trace.addr.begin == 0)
      throw std::runtime_error ("malloc: cannot allocate memory for log trace");

   // id operands
   malloc_memreg (&rt.trace.id, conf.tracesize * sizeof (uint16_t));
   rt.trace.idptr = (uint16_t *) rt.trace.id.begin;
   if (rt.trace.id.begin == 0)
      throw std::runtime_error ("malloc: cannot allocate memory for log trace");

   // val operands
   malloc_memreg (&rt.trace.val, conf.tracesize * sizeof (uint64_t));
   rt.trace.valptr = (uint64_t *) rt.trace.val.begin;
   if (rt.trace.val.begin == 0)
      throw std::runtime_error ("malloc: cannot allocate memory for log trace");

   // function _rt_start will save here the hosts's stack pointer
   rt.host_rsp = 0;

   // instrument the module to use this->rt as state
   DEBUG ("stid: executor: instrumenting constant pointers:");
   llvm::GlobalVariable *g;
   llvm::Type *t;
   std::string s;
   g = m->getGlobalVariable ("rt", true);
   t = m->getTypeByName ("struct.rt");
   if (!g or !t) throw std::runtime_error ("Executor: input missing runtime");
   g->setInitializer (ptr_to_llvm (&rt, t));
   print_value (g, s);
   DEBUG ("stid: executor: - %s", s.c_str());

   // similarly for the other const global variables
   std::vector<std::pair<const char*, uint64_t>> pairs =
      { std::make_pair ("memstart", (uint64_t) rt.mem.begin),
        std::make_pair ("memend", (uint64_t) rt.mem.end),
        std::make_pair ("evend", (uint64_t) rt.trace.ev.end) };
   for (auto &p : pairs)
   {
      g = m->getGlobalVariable (p.first, true);
      if (!g) throw std::runtime_error ("Executor: input missing runtime");
      g->setInitializer (llvm::ConstantInt::get
            (llvm::Type::getInt64Ty (ctx), p.second));
      s.clear();
      print_value (g, s);
      DEBUG ("stid: executor: - %s", s.c_str());
   }
}

void Executor::instrument_events ()
{
   // instrument the code
   Instrumenter i;
   if (not i.instrument (*m))
   {
      throw std::runtime_error ("Executor: rt missing in input module");
   }
}

void Executor::run ()
{
   void *ptr;
   int (*entry) (int, const char* const*, const char* const*);

   // make sure that argv and envp members have the right null pointer at the
   // end
   ASSERT (argv.size() >= 1 and argv[0] != 0);
   ASSERT (envp.size() and envp.back() == 0);

   // ask LLVM to JIT the program
   ee->finalizeObject ();
   ptr = (void *) ee->getFunctionAddress ("_rt_start");
   entry = (int (*) (int, const char* const*, const char* const*)) ptr;
   ASSERT (ptr);

   // allocation of the data segments takes place in finalizeObject(), so we
   // need to now correct (define) the heap memory region in rt
   rt.heap.begin = (uint8_t *) ALIGN16 (rt.data.end);
   rt.heap.end = rt.stacks.begin;
   rt.heap.size = rt.heap.end - rt.heap.begin;

   //DEBUG ("stid: rt       %16p", &rt);
   //DEBUG ("stid: memstart %16p", rt.mem.begin);
   //DEBUG ("stid: memend   %16p", rt.mem.end);
   //DEBUG ("stid: evmend   %16p", rt.trace.ev.end);

   // run the user program!!
   DEBUG ("stid: executor: starting guest execution");
   DEBUG ("stid: executor: ==========================================================");
   breakme ();
   exitcode = entry (argv.size(), argv.data(), envp.data());
   DEBUG ("stid: executor: ==========================================================");
   DEBUG ("stid: executor: guest execution terminated");
   DEBUG ("stid: executor: %zu events collected", rt.trace.size);
   ASSERT (rt.trace.size == (size_t) (rt.trace.evptr - (uint8_t*) rt.trace.ev.begin));
}

llvm::Constant *Executor::ptr_to_llvm (void *ptr, llvm::Type *t)
{
   llvm::Constant *c;

   // generate integer
   c = llvm::ConstantInt::get (llvm::Type::getInt64Ty (ctx), (uint64_t) ptr);
   // convert it into pointer to type t
   c = llvm::ConstantExpr::getIntToPtr (c, t->getPointerTo (0));
   return c;
}

struct rt *Executor::get_trace ()
{
   return &rt;
}
