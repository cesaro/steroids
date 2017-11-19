
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include <cstdio>
#include <vector>

#include "llvm/Pass.h"
#include "llvm/PassSupport.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/ExecutionEngine/MCJIT.h"

#include "stid/c/steroid.h"
#include "stid/executor.hh"

#include "pta/fixpoint.hh"
#include "pta/node-base.hh"

#include "verbosity.h"
#include "checker.hh"
#include "../rt/rt.h"
#include "../rt/lsd.h"
 
namespace stid {

void ir_write_ll (const llvm::Module *m, const char *filename)
{
   int fd = open (filename, O_WRONLY | O_TRUNC | O_CREAT, 0644);
   ASSERT (fd >= 0);
   llvm::raw_fd_ostream f (fd, true);
   f << *m;
}

void ir_write_bc (const llvm::Module *m, const char *filename)
{
   int fd = open (filename, O_WRONLY | O_TRUNC | O_CREAT, 0644);
   ASSERT (fd >= 0);
   llvm::raw_fd_ostream f (fd, true);
   llvm::WriteBitcodeToFile (m, f);
}

class HelloFunctionPass : public llvm::FunctionPass
{
public:
   static char ID;
   HelloFunctionPass () : llvm::FunctionPass (ID) {}

   bool runOnFunction (llvm::Function & f)
   {
      DEBUG ("pass %p f %p f.name '%s'", this, &f, f.getName ());
#if 0
      for (auto &b : f)
      {
         DEBUG ("  b %p name '%s' size %zd", &b, b.getName (), b.size ());
         for (auto &ins : b)
         {
            DEBUG ("     i %p", &ins);
            fflush (stdout);
            ins.dump ();
            llvm::errs().flush();
         }
      }
#else
      for (llvm::inst_iterator i = llvm::inst_begin (f), e = llvm::inst_end (f); i != e; ++i)
      {
         DEBUG ("  i %p", &*i);
         fflush (stdout);
         i->dump ();
         llvm::errs().flush();
      }
#endif
      return false;
   }
};

char HelloFunctionPass::ID = 0;
static llvm::RegisterPass<HelloFunctionPass> tmp
      ("cesar", "Hello function pass", false, false);

void test1 ()
{
   // get a context
   llvm::LLVMContext &context = llvm::getGlobalContext();
   llvm::SMDiagnostic err;

   // file to load and execute
   std::string path = "input.ll";

   // parse the .ll file and get a Module out of it
   std::unique_ptr<llvm::Module> mod (llvm::parseIRFile (path, err, context));
   llvm::Module * m = mod.get();

   // if errors found, write them to errors and return
   if (! mod.get ()) {
      std::string errors;
      llvm::raw_string_ostream os (errors);
      err.print (path.c_str(), os);
      os.flush ();
      printf ("Error: %s\n", errors.c_str());
      return;
   }

   printf ("functions in the module:\n");
#ifdef VERB_LEVEL_DEBUG
   for (auto & f : m->functions())
      DEBUG ("- m %p fun %p decl %d name %s",
            m, &f, f.isDeclaration(), f.getName().str().c_str());
#endif
   fflush (stdout);
   llvm::outs().flush();
   llvm::errs().flush();

   // create a function pass manager
   printf ("creating fpm:\n");
   fflush (stdout);
   llvm::legacy::FunctionPassManager fpm (m);

   // register a new pass
   fpm.add (new HelloFunctionPass ());

   // run our pass on all functions
   printf ("running the pass manager:\n");
   fflush (stdout);
   for (auto & f : m->functions())
   {
      //DEBUG ("- optimizing fun %p name %s", &f, f.getName ());
      //fflush (stdout);
      fpm.run (f);
   }
   fflush (stdout);
}

// test2 removed

void test3 ()
{
   // related to the JIT engine, can we move it to some static class constructor
   // of the Executor??
   llvm::InitializeNativeTarget();
   llvm::InitializeNativeTargetAsmPrinter();
   llvm::InitializeNativeTargetAsmParser();

   // get a context
   llvm::LLVMContext &context = llvm::getGlobalContext();
   llvm::SMDiagnostic err;
   std::string errors;

   // file to load and execute
   std::string path = "input.ll";
   //std::string path = "cunf.ll";
   //std::string path = "invalid.ll";

   // parse the .ll file and get a Module out of it
   std::unique_ptr<llvm::Module> mod (llvm::parseIRFile (path, err, context));
   llvm::Module * m = mod.get();

   // if errors found, report and terminate
   if (! mod.get ()) {
      llvm::raw_string_ostream os (errors);
      err.print (path.c_str(), os);
      os.flush ();
      printf ("Error: %s\n", errors.c_str());
      return;
   }

   printf ("functions in the module:\n");
#ifdef VERB_LEVEL_DEBUG
   for (auto & f : *m)
      DEBUG ("- m %p fun %p decl %d name %s",
            m, &f, f.isDeclaration(), f.getName().str().c_str());
#endif
   fflush (stdout);
   printf ("globals in the module:\n");
   for (auto & g : m->globals()) llvm::errs() << "- g " << &g << " dump " << g << "\n";
   fflush (stdout);

   // prepare an Executor, the constructor instruments and allocates guest
   // memory
   ExecutorConfig conf;
   conf.memsize = 1 << 30; // 1G
   conf.defaultstacksize = 16 << 20; // 16M
   conf.tracesize = 8 << 20; // 8M events (x 11 bytes per event)
   // FIXME: fill other fields of conf
   Executor e (std::move (mod), conf);

   // write instrumented code to file, for debugging purposes
   ir_write_ll (m, "out.ll");
   DEBUG ("stid: module saved to 'out.ll'");

   // prepare arguments for the program
   e.argv.push_back ("prog");
   e.argv.push_back ("a");
   e.argv.push_back ("b");
   e.argv.push_back ("c");
   e.environ.push_back ("HOME=/home/cesar");
   e.environ.push_back ("PWD=/usr/bin");
   e.environ.push_back (nullptr);

   e.run ();
   DEBUG ("stid: exitcode %d", e.exitcode);
   fflush (stdout);
   fflush (stderr);
   return;
}

void test4 ()
{
   llvm::SMDiagnostic err;
   std::string errors;
   llvm::LLVMContext &ctx = llvm::getGlobalContext();
   std::string path = "tests/hello.ll";

   printf ("test4\n");

   std::unique_ptr<llvm::Module> mod (llvm::parseIRFile (path, err, ctx));

   // if errors found, report and terminate
   if (! mod.get ()) {
      llvm::raw_string_ostream os (errors);
      err.print (path.c_str(), os);
      os.flush ();
      printf ("Error: %s\n", errors.c_str());
      return;
   }

   llvm::outs() << "mod name '" << mod->getName() << "'\n";
   //llvm::outs() << "mod data layout '" << mod->getDataLayoutStr() << "'\n";

   for (auto &f : mod->functions ())
   {
      llvm::errs() << "======================\n";
      llvm::errs() << f.getName() << "\n";
      f.getType()->dump();
      llvm::errs() << "users:\n";
      for (auto user : f.users ())
      {
         llvm::errs() << *user << "\n";
      }
   }
}

void test5 ()
{
   // related to the JIT engine, can we move it to some static class constructor
   // of the Executor??
   llvm::InitializeNativeTarget();
   llvm::InitializeNativeTargetAsmPrinter();
   llvm::InitializeNativeTargetAsmParser();

   // get a context
   llvm::LLVMContext &context = llvm::getGlobalContext();
   llvm::SMDiagnostic err;
   std::string errors;

   // file to load and execute
   std::string path = "input.ll";
   //std::string path = "cunf.ll";
   //std::string path = "invalid.ll";

   // parse the .ll file and get a Module out of it
   std::unique_ptr<llvm::Module> mod (llvm::parseIRFile (path, err, context));
   llvm::Module * m = mod.get();

   // if errors found, report and terminate
   if (! mod.get ()) {
      llvm::raw_string_ostream os (errors);
      err.print (path.c_str(), os);
      os.flush ();
      printf ("Error: %s\n", errors.c_str());
      return;
   }

#if 0
   printf ("functions in the module:\n");
   for (auto & f : *m)
      DEBUG ("- m %p fun %p decl %d name %s", m, &f, f.isDeclaration(), f.getName().str().c_str());
   fflush (stdout);
   printf ("globals in the module:\n");
   for (auto & g : m->globals()) llvm::errs() << "- g " << &g << " dump " << g << "\n";
   fflush (stdout);
#endif

   // prepare an Executor, the constructor instruments and allocates guest
   // memory
   ExecutorConfig conf;
   conf.memsize = 512 << 20; // 512M
   conf.defaultstacksize = 16 << 20; // 16M
   conf.tracesize = 16 << 20; // 16M events (x 11 bytes per event)
   conf.strace.fs = 1;
   // FIXME: fill other fields of conf
   Executor e (std::move (mod), conf);

   // write instrumented code to file, for debugging purposes
   ir_write_ll (m, "out.ll");
   DEBUG ("stid: module saved to 'out.ll'");

   // prepare arguments for the program
   e.argv.push_back ("cunf");
   e.argv.push_back ("/tmp/dme3.ll_net");
   //e.argv.push_back ("a");
   //e.argv.push_back ("b");
   //e.argv.push_back ("c");
   e.environ.push_back ("HOME=/home/cesar");
   e.environ.push_back ("PWD=/usr/bin");
   e.environ.push_back (nullptr);

   // run the guest
   e.run ();
   DEBUG ("stid: exitcode %d", e.exitcode);

   // prepare a stream object
   action_streamt actions (e.get_trace ());

   conft po (actions);

   // print it !
   //po.print_original_stream ();

   // build the partial order and print it
   //po.build ();
   //po.print ();

   fflush (stdout);
   fflush (stderr);
}

void test6 ()
{
   // related to the JIT engine, can we move it to some static class constructor
   // of the Executor??
   llvm::InitializeNativeTarget();
   llvm::InitializeNativeTargetAsmPrinter();
   llvm::InitializeNativeTargetAsmParser();

   // get a context
   llvm::LLVMContext &context = llvm::getGlobalContext();
   llvm::SMDiagnostic err;
   std::string errors;

   // file to load and execute
   std::string path = "input.ll";
   //std::string path = "cunf.ll";
   //std::string path = "invalid.ll";

   // parse the .ll file and get a Module out of it
   std::unique_ptr<llvm::Module> mod (llvm::parseIRFile (path, err, context));
   llvm::Module * m = mod.get();

   // if errors found, report and terminate
   if (! mod.get ()) {
      llvm::raw_string_ostream os (errors);
      err.print (path.c_str(), os);
      os.flush ();
      printf ("Error: %s\n", errors.c_str());
      return;
   }

   // prepare an Executor, the constructor instruments and allocates guest
   // memory
   ExecutorConfig conf;
   conf.memsize = 512 << 20; // 512M
   conf.defaultstacksize = 16 << 20; // 16M
   conf.tracesize = 16 << 20; // 16M events (x max 17 bytes per event)
   conf.optlevel = 3;

   conf.flags.dosleep = 0;
   conf.flags.verbose = 1;

   conf.strace.fs = 1;
   conf.strace.pthreads = 1;
   conf.strace.malloc = 1;
   conf.strace.proc = 1;
   conf.strace.others = 1;

   conf.do_load_store = false;

   Executor e (std::move (mod), conf);

   // write instrumented code to file, for debugging purposes
   ir_write_ll (m, "out.ll");
   DEBUG ("stid: test: module saved to 'out.ll'");

   // prepare arguments for the program
   //e.argv.push_back ("sssseq");
   e.argv.push_back ("sssort");
   e.argv.push_back ("f");
   //e.argv.push_back ("/tmp/dme3.ll_net");
   //e.argv.push_back ("-i");
   e.environ.push_back ("HOME=/home/cesar");
   e.environ.push_back ("PWD=/usr/bin");
   e.environ.push_back (nullptr);

   // run the guest
#if 0
   Replay replay;
   replay.push_back ({0, 1});  // #0 st
   replay.push_back ({0, 8});  // #0 c#1 c#2 l,u x 3 , but no exit
   replay.push_back ({1, 2});  // #1 s l
   //replay.push_back ({0, 1}); // -> error, cannot schedule the #0 exit
   replay.push_back ({-1, -1}); // free mode
#endif
#if 0
   Replay replay;
   replay.push_back ({0, 1});  // #0 st
   replay.push_back ({0, 1});  // #0 c#1
   replay.push_back ({1, 1 + 2 * 3}); // #1 st, lu x 3, stop before the exit
   replay.push_back ({0, 1});  // #0 c#2
   replay.push_back ({2, 1});
   replay.push_back ({-1, -1}); // free mode
#endif
#if 1
   Replay replay;
   replay.push_back ({-1, -1});
#endif

   e.set_replay (replay);
   //e.add_sleepset (0, (void*) 0x125);
   e.run ();
   action_streamt actions (e.get_trace ());
   action_stream2t s1 (actions);

   // print the stream and the replay
   actions.print ();
   actions.print_replay ();
   replay = actions.get_replay ();

#if 0
   DEBUG ("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");
   DEBUG ("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");
   DEBUG ("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");
   DEBUG ("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");
   DEBUG ("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");

   // run the guest 2nd time
   e.set_replay (replay.data(), (int) replay.size());

   e.run ();
   action_stream2t s2 (actions);
   actions.print ();
   actions.print_replay ();

   // diff
   printf ("\n");
   //s1.diff (s2, action_stream2t::optt::SPOT_FIRST);
   s1.diff (s2, action_stream2t::optt::FULL);
#endif
   // build a po and print it
   conft po (actions);
   //po.build ();
   po.print ();

   fflush (stdout);
   fflush (stderr);
   return;
}

void test7 ()
{
   // related to the JIT engine, can we move it to some static class constructor
   // of the Executor??
   llvm::InitializeNativeTarget();
   llvm::InitializeNativeTargetAsmPrinter();
   llvm::InitializeNativeTargetAsmParser();

   // get a context
   llvm::LLVMContext &context = llvm::getGlobalContext();
   llvm::SMDiagnostic err;
   std::string errors;

   // file to load and execute
   std::string path = "input.ll";
   //std::string path = "cunf.ll";
   //std::string path = "invalid.ll";

   // parse the .ll file and get a Module out of it
   std::unique_ptr<llvm::Module> mod (llvm::parseIRFile (path, err, context));
   llvm::Module *m = mod.get();

   // if errors found, report and terminate
   if (! mod.get ()) {
      llvm::raw_string_ostream os (errors);
      err.print (path.c_str(), os);
      os.flush ();
      printf ("Error: %s\n", errors.c_str());
      return;
   }

   llvm::Function *f = m->getFunction ("main16");
   ASSERT (f);

   pta::Fixpoint fp (*f);
   pta::State &s = fp.run ();
   s.dump();
}

} // namespace

int main (int argc, char **argv)
{
   stid::test6 ();
   return 0;
}
