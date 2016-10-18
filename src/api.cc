
#include <err.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

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

#include <steroid/steroid.h>

#undef DEBUG // exported by ExecutionEngine.h
#include "verbosity.h"

#include "executor.hh"
#include "checker.hh"
#include "test.hh"
#include <steroid/util/hsapi.h> 

// we export C bindings for this whole file
extern "C"
{

struct stid
{
   const char *path;
   Executor *e;
};

// constructor and destructor
struct stid * stid_init ()
{
   struct stid *s;
   s = (struct stid *) malloc (sizeof (struct stid));
   if (s == 0) return 0;
   s->path = 0;
   return s;
}

int stid_term (struct stid *s)
{
   free (s->e);
   free (s);
   return 0;
}

int stid_load_bytecode (struct stid *s, const char *path)
{
   printf ("stid_load_bytecode: loading %s\n", path);

   // related to the JIT engine
   llvm::InitializeNativeTarget();
   llvm::InitializeNativeTargetAsmPrinter();
   llvm::InitializeNativeTargetAsmParser();

   // get a context
   llvm::LLVMContext &context = llvm::getGlobalContext();
   llvm::SMDiagnostic err;
   std::string errors;

   // parse the .ll file and get a Module out of it
   std::unique_ptr<llvm::Module> mod (llvm::parseIRFile (path, err, context));

   // if errors found, report and terminate
   if (! mod.get ()) {
      llvm::raw_string_ostream os (errors);
      err.print (path, os);
      os.flush ();
      printf ("Error: %s\n", errors.c_str());
      return 1;
   }

   // prepare an Executor, the constructor instruments and allocates guest
   // memory
   ExecutorConfig conf;
   conf.memsize = 512 << 20; // 512M
   conf.stacksize = 16 << 20; // 16M
   conf.tracesize = 16 << 20; // 16M events (x 11 bytes per event)
   Executor *e = new Executor (std::move (mod), conf);

   // prepare arguments for the program
   e->argv.push_back ("cunf");
   e->argv.push_back ("/tmp/dme3.ll_net");
   //e->argv.push_back ("a");
   //e->argv.push_back ("b");
   //e->argv.push_back ("c");
   e->envp.push_back ("HOME=/home/msousa");
   e->envp.push_back ("PWD=/usr/bin");
   e->envp.push_back (nullptr);

   s->e = e;

   return 0;
}

// For now, this just calls the run of the executor 
int stid_run (struct stid *s, struct stid_replay *rep)
{
   s->e->run ();
   return 0;
}

int stid_get_seqexec (struct stid *s, struct stid_exec *run)
{
   ASSERT (s);
   ASSERT (run);
   da_init (&run->tab, struct stid_action);
   return 0;
}

int stid_convert_act_type (action_typet t)
{
   switch (t)
   {
     case action_typet::THCREAT : return STID_CREATE;
     case action_typet::THSTART : return STID_ENTRY;
     case action_typet::THEXIT  : return STID_EXIT;
     case action_typet::THJOIN  : return STID_JOIN;
     case action_typet::MTXLOCK : return STID_LOCK;
     case action_typet::MTXUNLK : return STID_UNLOCK;
     default :
       printf ("stid_convert_act_type: conversion failed\n");
       ASSERT (0);
       return 1;
   }
}

int stid_convert_po (conft pocc, struct stid_po *po)
{
   int num_ths = pocc.get_num_ths ();

   da_init (&po->procs, struct da);
   da_trunc (&po->procs, num_ths, struct da);

   struct da* p[num_ths];
   int es_proc = 0;
   eventt *e;
   eventt *other;

   printf ("stid_convert_po : going to start iterations\n");
   for (int i = 0; i < num_ths; i++)
   {
      p[i] = &da_i(&po->procs, i, struct da);
      da_init (p[i], struct stid_event);
      es_proc = pocc.events[i].size ();
      da_trunc (p[i], es_proc, struct stid_event);
      printf ("stid_convert_po : going through thread %2d with size %2d\n", i, es_proc);
      for (int j = 0; j < es_proc; j++)
      {
         e = &pocc.events[i][j];
         other = e->pre_other ();
         printf ("stid_convert_po : going through event %2d\n", j);

         da_i (p[i], j, struct stid_event).act.type = stid_convert_act_type (e->act.type);
         da_i (p[i], j, struct stid_event).act.addr = e->act.addr;
         da_i (p[i], j, struct stid_event).act.val = e->act.val;
         da_i (p[i], j, struct stid_event).sidx = e->sidx ();
         if (other)
         {
            da_i (p[i], j, struct stid_event).pre_mem.tid = other->tid (); 
            da_i (p[i], j, struct stid_event).pre_mem.idx = 0; //other->idx (pocc);
         }
         else
         {
            da_i (p[i], j, struct stid_event).pre_mem.tid = 0;
            da_i (p[i], j, struct stid_event).pre_mem.idx = 0;
         } 
      } 
   } 
   return 0;
}

struct stid_po * stid_get_poexec (struct stid *s)
{
   // prepare a stream object
   action_streamt actions (s->e->get_trace ());

   conft pocc (actions);

   // build the partial order 
   pocc.build ();
  
   pocc.print (); 
   struct stid_po *po;
   po = (struct stid_po *) malloc (sizeof (struct stid_po));
   if (po == 0) return 0;

   printf ("stid_get_poexec: exited build\n");
   stid_convert_po (pocc, po);
   return po;
}


int stid_test ()
{
   //printf ("stid_test: I feel fantastic!\n");
   //struct stid *s = stid_init ();
   //int r = stid_load_bytecode (s, "input.ll"); 
   //printf ("stid_test: result of load %2d\n", r);
   //stid_run (s, nullptr);
   //struct stid_po *po = stid_get_poexec (s);
   //printf ("stid_test: exited stid_get_poexec\n");
   //r = stid_print_po (*po); 
   //printf ("stid_test: result of print %2d\n", r);

   const char *user = getenv ("USER");

   user = 0;

   // for Cesar
   if (user and strcmp (user, "cesar") == 0)
   {
      test6 ();
      return 0;
   }

   // for anyone else
   test5 ();
   fflush(stdout);
   return 0;
}

int stid_test_checker ()
{
   //int res = test_checker ();
   int res = 0;
   test5 ();
   return res;
}

} // extern "C"
