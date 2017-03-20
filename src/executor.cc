
#include <vector>
#include <utility>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/ExecutionEngine/MCJIT.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/IPO.h"

#include "stid/action_stream.hh"
#include "stid/executor.hh"

#include "verbosity.h"
#include "misc.hh"
#include "../rt/rt.h"
#include "instrumenter.hh"

void dump_ll (const llvm::Module *m, const char *filename)
{
   int fd = open (filename, O_WRONLY | O_TRUNC | O_CREAT, 0644);
   ASSERT (fd >= 0);
   llvm::raw_fd_ostream f (fd, true);
   f << *m;
}


uint8_t *MyMemoryManager::allocateDataSection(uintptr_t Size, unsigned Alignment,
         unsigned SectionID, llvm::StringRef SectionName, bool isReadOnly)
{
   uint8_t *ptr;

   ptr = (uint8_t *) ALIGN16 ((uint64_t) rt.data.end);
   rt.data.end = ptr + Size;
   rt.data.size = rt.data.end - rt.data.begin;

   //ptr = llvm::SectionMemoryManager::allocateDataSection (Size, Alignment, SectionID, SectionName, isReadOnly);

   TRACE ("stid: executor: memory manager: allocation request: ptr %16p size %6zd align %d secid %d ro %d secname '%s'",
         ptr,
         Size, Alignment, SectionID, isReadOnly, SectionName.str().c_str());
   return ptr;
}

Executor::Executor (std::unique_ptr<llvm::Module> mod, ExecutorConfig c) :
   sleepsetidx (),
   conf (c),
   ctx (mod->getContext ()),
   m (mod.get()),
   ee (0),
   replay_capacity (2048) // 1024 context switches
{
   std::string errors;
   llvm::EngineBuilder eb (std::move(mod));

   // make sure we can safely destroy the object
   rt.mem.begin = 0;
   rt.trace.ev.begin = 0;
   rt.trace.addr.begin = 0;
   rt.trace.id.begin = 0;
   rt.trace.val.begin = 0;

   // reserve capacity in sleepsetidx
   sleepsetidx.reserve (RT_MAX_THREADS);

   // create a memory manager for the JIT engine (the builder owns the object)
   std::unique_ptr<llvm::RTDyldMemoryManager> mm (new MyMemoryManager (rt));

   // create a JIT execution engine, using the (new) MCJIT engine
   eb.setErrorStr (&errors);
   eb.setOptLevel (llvm::CodeGenOpt::Level::Aggressive);
   //eb.setVerifyModules (true);
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
   optimize ();
   jit_compile ();
   detex_init ();
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

void Executor::print_memreg (struct memreg *m, const char *prefix, const char *suffix)
{
   printf ("%s%p - %p, %4zu%s%s",
      prefix,
      m->begin,
      m->end,
      UNITS_SIZE (m->size),
      UNITS_UNIT (m->size),
      suffix);
}

void Executor::initialize_and_instrument_rt ()
{
   unsigned i;

   TRACE ("stid: executor: allocating guest memory");

   // main's stack should fit into the main memory
   if (conf.defaultstacksize >= conf.memsize)
   {
      std::string s = fmt ("stid: executor: requested stack size (%lu) is larger "
            "than requested memory size (%lu)",
            conf.defaultstacksize, conf.memsize);
      throw std::runtime_error (s.c_str());
   }
   rt.default_thread_stack_size = conf.defaultstacksize;

   // allocate the memory space for the guest code (data, rodata, bss, heap,
   // main stack, ...)
   malloc_memreg (&rt.mem, conf.memsize);
   if (rt.mem.begin == 0)
      throw std::runtime_error ("stid: executor: unable to malloc guest memory,"
            " malloc failed");

   // the main stack is located at the end of the memory
   rt.t0stack.size = conf.defaultstacksize;
   rt.t0stack.end = rt.mem.end;
   rt.t0stack.begin = rt.t0stack.end - rt.t0stack.size;

   // the data segment will be initialized when the JIT engine calls our
   // MyMemoryManager, which in turn grows this section to place the .data,
   // .bss, .rodata, etc. sections here
   rt.data.begin = rt.mem.begin;
   rt.data.end = rt.mem.begin;
   rt.data.size = 0;

   // allocate memory for the event stream, ids (uint8_t)
   malloc_memreg (&rt.trace.ev, conf.tracesize);
   if (rt.trace.ev.begin == 0)
      throw std::runtime_error ("stid: executor: unable to allocate memory for log trace");

   // addr operands
   malloc_memreg (&rt.trace.addr, conf.tracesize * sizeof (uint64_t));
   if (rt.trace.addr.begin == 0)
      throw std::runtime_error ("stid: executor: unable to allocate memory for log trace");

   // id operands
   malloc_memreg (&rt.trace.id, conf.tracesize * sizeof (uint16_t));
   if (rt.trace.id.begin == 0)
      throw std::runtime_error ("stid: executor: unable to allocate memory for log trace");

   // val operands
   malloc_memreg (&rt.trace.val, conf.tracesize * sizeof (uint64_t));
   if (rt.trace.val.begin == 0)
      throw std::runtime_error ("stid: executor: unable to allocate memory for log trace");

   // restart the pointers in the trace
   restart_trace ();

   // our initial replay sequence is just the vector [-1]
   ASSERT (replay_capacity >= 1);
   rt.replay.tab = new struct replayevent[replay_capacity];
   rt.replay.tab[0].tid = -1; // we start in free mode
   rt.replay.tab[0].count = -1;
   rt.replay.size = 1;
   for (i = 0; i < RT_MAX_THREADS; i++) rt.replay.sleepset[i] = 0;

   // function __rt_start will save here the hosts's stack pointer
   rt.host_rsp = 0;

   // instrument the module to use this->rt as state
   TRACE ("stid: executor: instrumenting constant pointers:");
   llvm::GlobalVariable *g;
   llvm::Type *t;
   std::string s;
   g = m->getGlobalVariable ("rt", true);
   t = m->getTypeByName ("struct.rt");
   if (!g or !t) throw std::runtime_error ("Executor: input missing runtime: no struct.rt type found");
   g->setInitializer (ptr_to_llvm (&rt, t));
   print_value (g, s);
   TRACE ("stid: executor: - %s", s.c_str());

   // similarly for the other const global variables
   std::vector<std::pair<const char*, uint64_t>> pairs =
      { std::make_pair ("memstart", (uint64_t) rt.mem.begin),
        std::make_pair ("memend", (uint64_t) rt.mem.end),
        std::make_pair ("evend", (uint64_t) rt.trace.ev.end) };
   for (auto &p : pairs)
   {
      g = m->getGlobalVariable (p.first, true);
      if (!g) throw std::runtime_error ("Executor: input missing runtime: mem{start,end} variables missing");
      g->setInitializer (llvm::ConstantInt::get
            (llvm::Type::getInt64Ty (ctx), p.second));
      s.clear();
      print_value (g, s);
      TRACE ("stid: executor: - %s", s.c_str());
   }
}

void Executor::optimize ()
{
   // This function optimizes the LLVM just before jitting it, after
   // instrumentation has been made. Instrumentation should have opened many
   // oportunities for optimization. The code is inspired by that of opt(1), see
   // https://github.com/llvm-mirror/llvm/blob/master/tools/opt/opt.cpp

   if (conf.optlevel == 0)
   {
      DEBUG ("stid: executor: skipping optimization");
      return;
   }
   DEBUG ("stid: executor: optimizing, optlevel %u", conf.optlevel);
   //dump_ll (m, "before.ll");

   // a module pass manager and a function pass manager, they will store the
   // sequence of optimization passes that we run; we will insert the
   // optimization passes using a PassManagerBuilder
   llvm::legacy::PassManager pm;
   llvm::legacy::FunctionPassManager fpm (m);

#if 0
   fpm.add (llvm::createInstructionCombiningPass ());
   fpm.add (llvm::createReassociatePass ());
   fpm.add (llvm::createGVNPass ());
   fpm.add (llvm::createCFGSimplificationPass ());
#endif

   // create a builder, and set it up
   unsigned sizelevel = 0; // no -Os
   llvm::PassManagerBuilder builder;
   builder.OptLevel = conf.optlevel;
   builder.SizeLevel = sizelevel;
   builder.Inliner = llvm::createFunctionInliningPass (conf.optlevel, sizelevel);

   // fill the pass managers with the standard compiler optimizations
   builder.populateFunctionPassManager (fpm);
   builder.populateModulePassManager (pm);

   // run the function pass manager
   fpm.doInitialization ();
   for (llvm::Function &f : *m)
   {
      fpm.run (f);
   }
   fpm.doFinalization ();

   // run the module pass manger
   pm.run (*m);

   //dump_ll (m, "after.ll");
   DEBUG ("stid: executor: done");
}

void Executor::restart_trace ()
{
   //DEBUG ("stid: executor: restarting pointers in the action/replay streams");
   rt.trace.evptr = rt.trace.ev.begin;
   rt.trace.addrptr = (uint64_t *) rt.trace.addr.begin;
   rt.trace.idptr = (uint16_t *) rt.trace.id.begin;
   rt.trace.valptr = (uint64_t *) rt.trace.val.begin;

   rt.trace.size = 0; // "wrong" default value
   rt.trace.num_ths = -1; // "wrong" default value
   for (int i = 0; i < RT_MAX_THREADS; i++) rt.trace.num_blue[i] = 0;

   rt.replay.current = rt.replay.tab;
}

void Executor::jit_compile ()
{
   void *ptr;
   TRACE ("stid: executor: jit compiling ...");

   // ask LLVM to JIT the program
   ee->finalizeObject ();
   ptr = (void *) ee->getFunctionAddress ("__rt_start");
   entry = (int (*) (int, const char* const*, const char* const*)) ptr;
   if (entry == 0)
   {
      throw std::runtime_error ("stid: executor: unable to find entry symbol (__rt_start) after JIT compilation");
   }

   // allocation of the data segments takes place in finalizeObject(), so we
   // need to now correct (define) the heap memory region in rt
   rt.heap.begin = (uint8_t *) ALIGN16 (rt.data.end);
   rt.heap.end = rt.t0stack.begin;
   rt.heap.size = rt.heap.end - rt.heap.begin;

   INFO ("stid: executor: ready: %zu%s total memory, "
         "%zu%s data, %zu%s heap, %zu%s default stack size",
         UNITS_SIZE (rt.mem.size),
         UNITS_UNIT (rt.mem.size),
         UNITS_SIZE (rt.data.size),
         UNITS_UNIT (rt.data.size),
         UNITS_SIZE (rt.heap.size),
         UNITS_UNIT (rt.heap.size),
         UNITS_SIZE (rt.default_thread_stack_size),
         UNITS_UNIT (rt.default_thread_stack_size));

   TRACE ("stid: executor: entry function (__rt_start) at %p", entry);

   //DEBUG ("stid: executor: rt       %18p", &rt);
   //DEBUG ("stid: executor: memstart %18p", rt.mem.begin);
   //DEBUG ("stid: executor: memend   %18p", rt.mem.end);
   //DEBUG ("stid: executor: evend    %18p", rt.trace.ev.end);

   TRACE ("stid: executor: guest's address space:");
   if (verb_trace) {
      print_memreg (&rt.mem, "stid: executor:  ", ", total guest memory\n");
      print_memreg (&rt.data, "stid: executor:  ", ", data (.data, .bss, .rodata, and others)\n");
      print_memreg (&rt.heap, "stid: executor:  ", ", heap\n");
      print_memreg (&rt.t0stack, "stid: executor:  ", ", stack (main thread)\n");
   }

   TRACE ("stid: executor: event trace buffer:");
   if (verb_trace) {
      print_memreg (&rt.trace.ev, "stid: executor:  ", ", event trace (8bit event ids)\n");
      print_memreg (&rt.trace.addr, "stid: executor:  ", ", event trace (64bit addr)\n");
      print_memreg (&rt.trace.val, "stid: executor:  ", ", event trace (64bit val)\n");
      print_memreg (&rt.trace.id, "stid: executor:  ", ", event trace (16bit call ids)\n");
   }
}

void Executor::instrument_events ()
{
   // instrument the code
   Instrumenter i;
   TRACE ("stid: executor: instrumenting source...");
   if (not i.instrument (*m))
   {
      throw std::runtime_error ("stid: executor: rt missing in input module");
   }
   TRACE ("stid: executor: done");
}

void Executor::detex_init ()
{
   detex.rseed = (unsigned) time (0);
   detex.dataseg.clear ();
}

void Executor::detex_apply ()
{
   //DEBUG ("stid: executor: detex: starting");
   //DEBUG ("stid: executor: detex: srand(3) seed %u", detex.rseed);
   srand (detex.rseed);

   // if this is the first call, we save the program data segments for tuture
   // re-executions of the guest; if it is not, we restore the data segments
   // from the first execution
   if (detex.dataseg.size() == 0)
   {
      //DEBUG ("stid: executor: detex: preserving original JITed data segments, %zu B",
      //      rt.data.size);
      detex.dataseg.resize (rt.data.size);
      memcpy (detex.dataseg.data(), rt.data.begin, rt.data.size);
      detex.dataseg.shrink_to_fit();
   } else {
      //DEBUG ("stid: executor: detex: restoring JITed data segments, %zu B",
      //      detex.dataseg.size());
      ASSERT (detex.dataseg.size() == rt.data.size);
      memcpy (rt.data.begin, detex.dataseg.data(), rt.data.size);
   }

   // we should clear memory here
   //DEBUG ("stid: executor: detex: clearing memory out...");
   //memset (rt.heap.begin, 0, rt.heap.size);
	//memset (rt.t0stack.begin, 0, rt.t0stack.size);
   
   // restart optget(3)
   optind = 1;

   //DEBUG ("stid: executor: detex: done");
}

void Executor::run ()
{
   // reinitialize the action stream
   restart_trace ();

   // clear memory for deterministic execution
   detex_apply ();

   // make sure that argv and envp members have the right null pointer at the
   // end
   //DEBUG ("stid: executor: checking argv, argp");
   if (argv.size() == 0)
   {
      std::string s = fmt ("stid: executor: argv needs to contain at least one argument");
      throw std::runtime_error (s);
   }
   if (argv[0] == 0)
   {
      std::string s = fmt ("stid: executor: argv[0] cannot be a null pointer");
      throw std::runtime_error (s);
   }
   if (envp.size() == 0)
   {
      std::string s = fmt ("stid: executor: envp needs to contain at least one entry");
      throw std::runtime_error (s);
   }
   if (envp.back() != 0)
   {
      std::string s = fmt ("stid: executor: envp: last entry should be a null pointer");
      throw std::runtime_error (s);
   }

   // run the user program!!
   DEBUG ("stid: executor: starting guest execution");
   //DEBUG ("stid: executor: ====================================================");
   breakme ();
   exitcode = entry (argv.size(), argv.data(), envp.data());
   //DEBUG ("stid: executor: ====================================================");
   //DEBUG ("stid: executor: guest execution terminated");
   DEBUG ("stid: executor: %zu events collected, %d thread created, exitcode %d",
         rt.trace.size, rt.trace.num_ths, exitcode);
   ASSERT (rt.trace.size == (size_t) (rt.trace.evptr - (uint8_t*) rt.trace.ev.begin));
   ASSERT (rt.trace.num_ths >= 1);
   ASSERT (rt.trace.num_ths <= RT_MAX_THREADS);
   for (int i = 0; i < rt.trace.num_ths; i++) ASSERT (rt.trace.num_blue[i]);
   if (rt.trace.size == conf.tracesize)
      PRINT ("stid: executor: WARNING: event stream size exceeded, execution truncated!");
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

void Executor::set_replay (const struct replayevent *tab, int size)
{
   if (size > replay_capacity)
   {
      replay_capacity = size * 1.5;
      delete rt.replay.tab;
      rt.replay.tab = new struct replayevent[replay_capacity];
   }

   rt.replay.size = size;
   memcpy (rt.replay.tab, tab, size * sizeof(struct replayevent));
}

void Executor::add_sleepset (unsigned tid, void *addr)
{
   ASSERT (tid < RT_MAX_THREADS);
   sleepsetidx.push_back (tid);
   rt.replay.sleepset[tid] = (pthread_mutex_t *) addr;
}

void Executor::clear_sleepset ()
{
   for (auto i : sleepsetidx) rt.replay.sleepset[i] = 0;
}

struct rt *Executor::get_runtime ()
{
   return &rt;
}

action_streamt Executor::get_trace ()
{
   return action_streamt (&rt);
}

