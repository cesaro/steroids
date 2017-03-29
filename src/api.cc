
#include <err.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <exception>

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

#include "stid/c/util/hsapi.h"
#include "stid/c/steroid.h"
#include "stid/executor.hh"

#include "verbosity.h"
#include "checker.hh"
#include "test.hh"

// we export C bindings for this whole file
extern "C"
{

using namespace stid;

struct stid_handle
{
   const char *path;
   Executor *e;
   std::vector<std::string> storage;
};

static void _ir_write_ll (const llvm::Module *m, const char *filename)
{
   int fd = open (filename, O_WRONLY | O_TRUNC | O_CREAT, 0644);
   ASSERT (fd >= 0);
   llvm::raw_fd_ostream f (fd, true);
   f << *m;
}


// constructor and destructor
struct stid_handle * stid_init ()
{
   struct stid_handle *s;

   try {
      s = new struct stid_handle;  
   } catch (const std::bad_alloc& e) {
      return 0;
   }
   s->path = 0;
   s->e = 0;
   return s;
}

int stid_term (struct stid_handle *s)
{
   ASSERT (s);
   delete s->e;
   delete s;
   return 0;
}

int stid_load_bytecode (struct stid_handle *s, const char *path)
{
   static bool init = false;
   llvm::SMDiagnostic err;
   llvm::Module *m;
   std::string errors;

   ASSERT (s);
   ASSERT (path);
   ASSERT (s->path == 0);
   ASSERT (s->e == 0);
   DEBUG ("stid: load-bytecode: s %p path '%s'", s, path);

   // create a copy of the path, store it in s
   s->storage.push_back (path);
   s->path = s->storage.back().c_str();

   // related to the JIT engine
   if (not init)
   {
      init = true;
      llvm::InitializeNativeTarget();
      llvm::InitializeNativeTargetAsmPrinter();
      llvm::InitializeNativeTargetAsmParser();
   }

   // get a context
   llvm::LLVMContext &context = llvm::getGlobalContext();

   // parse the .ll file and get a Module out of it
   std::unique_ptr<llvm::Module> mod (llvm::parseIRFile (path, err, context));
   m = mod.get();

   // if errors found, report and terminate
   if (! mod.get ()) {
      llvm::raw_string_ostream os (errors);
      err.print (path, os);
      os.flush ();
      printf ("stid: load-bytecode: '%s': %s\n", path, errors.c_str());
      return -1;
   }

   // prepare an Executor, the constructor instruments and allocates guest
   // memory
   // FIXME - move this to a proper API for the stid struct
   ExecutorConfig conf;
   conf.memsize = 128 << 20;
   conf.defaultstacksize = 8 << 20;
   conf.tracesize = 16 << 20;

   DEBUG ("stid: load-bytecode: creating a bytecode executor...", s, path);
   try {
      s->e = new Executor (std::move (mod), conf);
   } catch (const std::exception &e) {
      printf ("stid: load-bytecode: errors preparing the bytecode executor\n");
      printf ("stid: load-bytecode: %s\n", e.what());
      goto err_executor;
   }
   DEBUG ("stid: load-bytecode: executor created with success!", s, path);

   // FIXME - this should be moved to a proper API
   s->e->environ.push_back ("HOME=/home/msousa");
   s->e->environ.push_back ("PWD=/usr/bin");
   s->e->environ.push_back (nullptr);

   s->e->argv.push_back ("program-name");
   s->e->argv.push_back ("argv1");
   s->e->argv.push_back ("argv2");

   DEBUG ("stid: load-bytecode: saving instrumented code to /tmp/output.ll");
   _ir_write_ll (m, "/tmp/output.ll");

   DEBUG ("stid: load-bytecode: done!", s, path);
   return 0;

err_executor :
   s->path = 0;
   s->e = 0;
   return -1;
}

void stid_argv_add (struct stid_handle *s, const char *arg)
{
   ASSERT (s);
   ASSERT (arg);
   ASSERT (s->e != 0);

   DEBUG ("stid: argv-add: s %p arg '%s'", s, arg);
   s->storage.push_back (arg);
   s->e->argv.push_back (s->storage.back().c_str());
}

void stid_argv_clear (struct stid_handle *s)
{
   ASSERT (s);
   ASSERT (s->e != 0);

   DEBUG ("stid: argv-clear: s %p", s);
   s->e->argv.clear();
}

int stid_run (struct stid_handle *s, struct stid_replay *rep)
{
   std::vector<int> replay;

   ASSERT (s);
   ASSERT (s->e);

   DEBUG ("stid: run: s %p replay %p", s);
   // build a replay for the Executor
   if (not rep)
   {
      replay = {-1};
   }
   else
   {
      for (int i = 0; i < rep->tab.len; i++)
      {
         replay.push_back (da_i (&rep->tab, i, struct stid_ctsw).thid);
         replay.push_back (da_i (&rep->tab, i, struct stid_ctsw).nrev);
      }
      replay.push_back (-1);
   }

   // The C API to set the replay is now broken following the changes performed
   // on the replay data structure, we need to updated it.
   printf ("FIXME - The API to set replays is now broken!!!\n");
   exit (1);
   //s->e->set_replay (replay.data(), replay.size());

   DEBUG ("stid: run: running the guest ...");

   // run the guest
   s->e->run ();
   DEBUG ("stid: run: done!"); 
   return 0;
}

int stid_get_seqexec (struct stid_handle *s, struct stid_exec *run)
{
   ASSERT (s);
   ASSERT (run);
   da_init (&run->tab, struct stid_action);
   return 0;
}

static int _stid_convert_act_type (action_typet t)
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
       printf ("_stid_convert_act_type: conversion failed\n");
       ASSERT (0);
       return 1;
   }
}

static void _stid_convert_po (const conft &pocc, struct stid_po *po)
{
   int num_ths = pocc.get_num_ths ();

   da_init (&po->procs, struct da);
   da_trunc (&po->procs, num_ths, struct da);

   struct da* p[num_ths];
   int es_proc = 0;
   const eventt *e;
   const eventt *other;

   for (int i = 0; i < num_ths; i++)
   {
      p[i] = &da_i(&po->procs, i, struct da);
      da_init (p[i], struct stid_event);
      es_proc = pocc.events[i].size ();
      da_trunc (p[i], es_proc, struct stid_event);
      for (int j = 0; j < es_proc; j++)
      {
         e = &pocc.events[i][j];
         other = e->pre_other ();
         //printf ("stid_convert_po: eventt %18p other %18p\n", e, other);

         da_i (p[i], j, struct stid_event).act.type = _stid_convert_act_type (e->act.type);
         da_i (p[i], j, struct stid_event).act.addr = e->act.addr;
         da_i (p[i], j, struct stid_event).act.val = e->act.val;
         da_i (p[i], j, struct stid_event).sidx = e->sidx ();
         if (other)
         {
            da_i (p[i], j, struct stid_event).pre_mem.tid = other->tid (); 
            da_i (p[i], j, struct stid_event).pre_mem.idx = other->idx (pocc);
         }
         else
         {
            da_i (p[i], j, struct stid_event).pre_mem.tid = 0;
            da_i (p[i], j, struct stid_event).pre_mem.idx = 0;
         } 
      } 
   }

   // generates the max_lock dynamic array 
   int num_mut = pocc.mutexmax.size ();
   struct da *a;
   da_init (&po->max_lock, struct stid_event);
   da_trunc (&po->max_lock, num_mut, struct stid_event);
   int i = 0;
   for (auto el : pocc.mutexmax)
   {
      a = &po->max_lock;
      e = el.second;
      other = e->pre_other ();
      da_i (a, i, struct stid_event).act.type = _stid_convert_act_type (e->act.type);
      da_i (a, i, struct stid_event).act.addr = e->act.addr;
      da_i (a, i, struct stid_event).act.val = e->act.val;
      da_i (a, i, struct stid_event).sidx = e->sidx ();
      if (other)
      {
         da_i (a, i, struct stid_event).pre_mem.tid = other->tid (); 
         da_i (a, i, struct stid_event).pre_mem.idx = other->idx (pocc);
      }
      else
      {
         da_i (a, i, struct stid_event).pre_mem.tid = 0;
         da_i (a, i, struct stid_event).pre_mem.idx = 0;
      }
      i++; 
   }
}

struct stid_po * stid_po_get (struct stid_handle *s)
{
   struct stid_po *po;
   DEBUG ("stid: po-get: s %p", s);

   // prepare a stream object
   action_streamt actions (s->e->get_trace ());

   // build the partial order 
   DEBUG ("stid: po-get: building the conft partial order ...");
   conft pocc (actions);
   pocc.build ();
   //pocc.print (); 
   DEBUG ("stid: po-get: done");
  
   // allocate memory for the po
   try {
      po = new struct stid_po;  
   } catch (const std::bad_alloc& e) {
      return 0;
   }

   DEBUG ("stid: po-get: converting conft -> stid_po");
   _stid_convert_po (pocc, po);
   DEBUG ("stid: po-get: done, returning");
   return po;
}

const char *stid_action_type2str (int a)
{
   switch (a)
   {
   // threads
   case STID_CREATE : return "STID_CREATE";
   case STID_JOIN   : return "STID_JOIN  ";
   case STID_ENTRY  : return "STID_ENTRY ";
   case STID_EXIT   : return "STID_EXIT  ";
   // locks
   case STID_LOCK   : return "STID_LOCK  ";
   case STID_UNLOCK : return "STID_UNLOCK";
   default :
      ASSERT (0);
      return "FAIL";
   }
}

int stid_action_print (struct stid_action *act)
{
   if (act == 0)
   {
      printf ("action: %18p type %s addr %18s val %18s\n", 
         act, "???        ", "???", "???");
      return -1;
   }
   printf ("action: %p type %s addr %#lx val %#lx\n", 
      act, stid_action_type2str(act->type), act->addr, act->val);
   return 0;
}

int stid_event_print (const struct stid_event *e,
      unsigned int tid, unsigned int idx)
{
   if (e == 0)
   {
      printf (" Event %4u this %18p tid %2u sidx %5s ac.type %s "
            "pre_mem { tid %2s idx %4s }\n",
            idx,
            e,
            tid,
            "??",
            "??",
            "??",
            "??");
      return -1;
   }

   printf (" Event %4u this %18p tid %2u sidx %5u ac.type %s "
         "pre_mem { tid %2u idx %4u }\n",
         idx,
         e,
         tid,
         e->sidx,
         stid_action_type2str (e->act.type),
         e->pre_mem.tid,
         e->pre_mem.idx);
   return 0;
}

int stid_po_print (const struct stid_po *po)
{
   struct da *p;
   struct stid_event *e;
   struct stid_event *e1;

   printf ("== po exec begin ==\n");

   for (int tid = 0; tid < po->procs.len; tid++)
   {
      p = &da_i (&po->procs, tid, struct da);
      e = &da_i (p, 0, struct stid_event); 
      e1 = &da_i (p, p->len-1, struct stid_event); 
      printf ("Thread %d: %u events, first %p, last %p\n", 
            tid, p->len, e, e1);

      for (int tlen = 0; tlen < p->len; tlen++)
      {
        stid_event_print (e++, tid, tlen);
      }
   }

   printf ("== po exec end ==\n");
   return 0; 
}

int stid_po_term (struct stid_po *po)
{
   delete po;
   return 0;
}

int stid_cmd (struct stid_handle *s, int cmd, void *arg1, void *arg2, void *arg3)
{
   switch (cmd)
   {
   case STID_CMD1 :
   {
      // print the sequential stream of actions and the replay
      action_streamt stream (s->e->get_trace ());
      stream.print ((int) * (int*) arg1);
      stream.print_replay ();
      // print the partial order
      conft pocc (stream);
      pocc.build ();
      pocc.print (); 
      return 0;
   }
  
   default :
      ASSERT (0);
   }
   return 0;
}

int stid_test ()
{

#if 0
   const char *user = getenv ("USER");
   // for Cesar
   if (user and strcmp (user, "cesar") == 0)
   {
      test6 ();
      return 0;
   }
#endif

   //// for anyone else
   test6 ();

#if 0
   printf ("stid_test: I feel fantastic!\n");
   struct stid *s = stid_init ();
   int r = stid_load_bytecode (s, "input.ll"); 
   printf ("stid_test: result of load %2d\n", r);
   stid_run (s, nullptr);
   struct stid_po *po = stid_get_poexec (s);
   printf ("stid_test: exited stid_get_poexec\n");
   r = stid_print_po (po); 
   printf ("stid_test: result of print %2d\n", r);
   fflush(stdout);
#endif
   return 0;
}

int stid_test_checker ()
{
   //int res = test_checker ();
   int res = 0;
   test6 ();
   return res;
}

} // extern "C"
