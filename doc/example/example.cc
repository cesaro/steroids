
#include <stdio.h>
#include <assert.h>

#include "llvm/IR/Module.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetSelect.h"

#include "stid/executor.hh"
#include "stid/verbosity.h"

#define MB (1024 * 1024)
#define NAME (argv[0])

int main (int argc, char **argv)
{
   llvm::LLVMContext context;
   llvm::SMDiagnostic err;
   stid::Executor *ex;
   std::string errors;
   const char *path;
   stid::Replay replay;

   // the first command-line argument is the path to the LLVM bitcode under
   // analysis
   if (argc != 2) return 1;
   path = argv[1];

   // LLVM initialization, related to the LLVM JIT engine
   llvm::InitializeNativeTarget();
   llvm::InitializeNativeTargetAsmPrinter();
   llvm::InitializeNativeTargetAsmParser();

   // parse a bitcode file and get a llvm::Module representing it
   printf ("%s: loading '%s' ...\n", NAME, path);
   std::unique_ptr<llvm::Module> mod (llvm::parseIRFile (path, err, context));

   // if errors found during the bitcode parsing then report and terminate
   if (! mod.get ())
   {
      llvm::raw_string_ostream os (errors);
      err.print (path, os);
      os.flush ();
      printf ("%s: '%s': %s\n", NAME, path, errors.c_str());
      return 1;
   }

   // Set up the verbosity level of steroids to the minimum possible (set this
   // to e.g. VERB_DEBUG to display steroids' debug messages), see
   // stid/verbosity.h and stid/c/config.h
   verb_set (VERB_DEBUG);

   // Configure a steroids executor to confine the execution of the program
   // analysis in 64M (text, data, heaps, bss, and stacks), using 4M for the
   // default thread stack size, optimizing the program with -O2, printing
   // strace(1)-like information on pthread_*, malloc/free library calls, and
   // reporting in the output stream load/store memory operations.
   stid::ExecutorConfig config;
   config.memsize = 64 * MB;
   config.defaultstacksize = 4 * MB;
   config.optlevel = 2;
   config.flags.verbose = 0; // set to 1 for the runtime to be verbose
   config.strace.fs = 1;
   config.strace.pthreads = 1;
   config.strace.malloc = 1;
   config.do_load_store = true;

   // Get an stid::Executor for the llvm moudle. This will internally optimize
   // and instrument the bitcode
   printf ("%s: creating an executor...\n", NAME);
   try
   {
      ex = new stid::Executor (std::move (mod), config);
   } catch (const std::exception &e)
   {
      printf ("%s: error: %s\n", NAME, e.what());
      return 1;
   }
   assert (ex);

   // set the environment variables for the program under analysis
   ex->environ.push_back ("HOME=/home/user");
   ex->environ.push_back ("PWD=/tmp");
   ex->environ.push_back (nullptr);

   // set the commandline arguments of the program under anlaysis
   ex->argv.push_back ("program-name");
   ex->argv.push_back ("argv1");
   ex->argv.push_back ("argv2");

   // The replay sequence tells the Executor when to context-switch between
   // threads. Initializnig it to {-1, -1} sets the Executor in the so-called
   // "free mode", where it can freely decide when threads context-switch. This
   // is useful for for the very first time we run the program, as such sequence
   // is difficult to calculate statically.
   replay.clear ();
   replay.push_back ({-1, -1});
   ex->set_replay (replay);

   // JIT compile, run the program, and print the exit code
   printf ("%s: running program ...\n", NAME);
   ex->run ();
   printf ("%s: execution terminated with exit code %d\n", NAME, ex->exitcode);

   // print the stream of actions produced by the library
   ex->get_trace().print ();

   // We now run again the program using a different replay sequence, where
   // thread 1 enter the critical section before thread 0, thus triggering an
   // assertion violation
   // 0 2; 1 2; -1
   replay.clear ();
   replay.push_back ({0, 2}); // thread 0, 2 actions (start thread, pthread_create)
   replay.push_back ({1, 2}); // thread 1, 2 actions (start thread, pthread_mutex_lock)
   replay.push_back ({-1, -1}); // switch to free mode
   ex->set_replay (replay);

   printf ("%s: running program again...\n", NAME);
   ex->run ();
   printf ("%s: execution terminated with exit code %d\n", NAME, ex->exitcode);
   ex->get_trace().print ();

   return 0;
}
