
#include <vector>
#include <utility>
#include <stdlib.h>

#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetSelect.h"

#undef DEBUG // exported by ExecutionEngine.h
#include "verbosity.h"
#include "misc.hh"
#include "../rt/rt.h"
#include "instrumenter.hh"

bool Instrumenter::instrument (llvm::Module &m)
{
   // cleanup
   reset (m);
   if (not find_rt ()) return false;

   // compute a map describing how we are supposed to substitute calls to
   // certain functions, as well as a numeric id for every function
   DEBUG ("stid: instrumenter: initializing substmap and funids mappings");
   init_maps ();

   // instrument every function
   DEBUG ("stid: instrumenter: starting the instrumentation of every function");
   for (auto &f : m)
   {
      if (is_rt_fun (&f) or f.isDeclaration ()) continue;

      DEBUG ("stid: instrumenter: === at fun %s",
            f.getName().str().c_str());

      // instrument the CALL event at the beginning of the entry block
      llvm::IRBuilder<> b (&f.getEntryBlock ().front());
      b.CreateCall (call, b.getInt16 (funids[&f]));

      // visit all instructions and instrument appropriately
      count = 0;
      visit (f);
      DEBUG ("stid: instrumenter: done, %d instructions instrumented", count);
   }

   // check that we didn't do anything stupid
   DEBUG ("stid: instrumenter: verifying module after instrumentation ...");
   llvm::verifyModule (m, &llvm::outs());
   DEBUG ("stid: instrumenter: done");

   return true;
}

bool Instrumenter::find_rt ()
{
   load_pre  = m->getFunction ("_rt_load_pre");
   load_post = m->getFunction ("_rt_load_post");
   store_pre  = m->getFunction ("_rt_store_pre");
   store_post = m->getFunction ("_rt_store_post");

   allo = m->getFunction ("_rt_allo");
   mllo = m->getFunction ("_rt_mllo");
   rllo = m->getFunction ("_rt_rllo");
   free = m->getFunction ("_rt_fre");
   call = m->getFunction ("_rt_call");
   ret  = m->getFunction ("_rt_ret");

   return load_pre != nullptr and store_post != nullptr;
}

bool Instrumenter::is_rt_fun (llvm::Function *f)
{
   return f->getName().startswith ("_rt_");
   #if 0
         or
         f->getName().equals ("free") or
         f->getName().equals ("malloc") or
         f->getName().equals ("realloc") or
         f->getName().equals ("calloc");
         #endif
}

void Instrumenter::reset (llvm::Module &m)
{
   this->m = &m;
   this->ctx = &m.getContext ();

   // clean the funids map
   next_call_id = 0;
   funids.clear ();

   // clear the function substitution map
   substmap.clear ();
}

void Instrumenter::init_maps ()
{
   std::string s;
   int i = 0;

   // here
   for (auto &f : *m)
   {
      // associate a unique numeric identifier to every function declaration or
      // definition
      funids[&f] = i++;

      // for every function named _rt_*, we get a substitution table
      if (not is_rt_fun (&f)) continue;
      s = f.getName ().substr(4);;
      llvm::Function *ff = m->getFunction (s); // stripping the "_rt_" prefix
      if (not ff) continue;
      DEBUG ("stid: instrumenter: init maps: substmap: %s %-20s -> %s",
               ff->isDeclaration() ? "decl" : "fun ",
               ff->getName().str().c_str(),
               f.getName().str().c_str());
      substmap[ff] = &f;
      if (f.getType () != ff->getType ())
      {
         DEBUG ("stid: instrumenter: init maps: substmap: WARNING: type mismatch between fun and rtfun:");
         s.clear ();
         print_type (ff->getFunctionType(), s);
         DEBUG ("stid: instrumenter: init maps: substmap:  fun  : %s", s.c_str());
         s.clear ();
         print_type (f.getFunctionType(), s);
         DEBUG ("stid: instrumenter: init maps: substmap:  rtfun: %s", s.c_str());
      }
   }

   // print function ids
   for (auto &p : funids)
   {
      DEBUG ("stid: instrumenter: init maps: funids: id %#5x %s %s",
            p.second,
            p.first->isDeclaration() ? "decl" : "fun ",
            p.first->getName().str().c_str());
   }
}

void Instrumenter::visitLoadInst (llvm::LoadInst &i)
{
   llvm::IRBuilder<> b (&i); // we instrument BEFORE the load instruction
   llvm::Value *addr;
   uint64_t size;

   //llvm::outs() << "stid: " << i << "\n";

   // get the address from where we want to load and bitcast it to i8*
   addr = i.getPointerOperand ();
   addr = b.CreateBitCast (addr, b.getInt8PtrTy ());

   // compute the size of the loaded value, according to the DataLayaout
   size = m->getDataLayout().getTypeStoreSize (i.getType());

   // make sure size has not a crazy value
   switch (size)
   {
   case 1 :
   case 2 :
   case 4 :
   case 8 :
      break;
   default :
      if (size & 7)
      {
         int ssize = (size + 8) & -8;
         std::string s;
         print_value (&i, s);
         DEBUG ("stid: instrumenter: WARNING: casting %u bit load to %u bit load:",
               8 * size, 8 * ssize);
         size = ssize;
         DEBUG ("stid: instrumenter:  %s", s.c_str());
      }
   }

   // instruction to call to the runtime
   b.CreateCall (load_pre, {addr, b.getInt32 (size)});

   // reattach the builder after the instruction and instrument a second call
   b.SetInsertPoint (i.getNextNode ());
   b.CreateCall (load_post, {addr, b.getInt32 (size)});

   count++;
}

void Instrumenter::visitStoreInst (llvm::StoreInst &i)
{
   llvm::IRBuilder<> b (&i); // we instrument BEFORE the store instruction
   llvm::Value *addr;
   llvm::Value *v;
   llvm::Type *t;
   uint64_t size;

   //llvm::outs() << "stid: " << i << "\n";

   // get the address where we want to store and bitcast it to i8*
   addr = i.getPointerOperand ();
   //t = llvm::Type::getInt8PtrTy (*ctx, addr->getType()->getPointerAddressSpace());
   t = b.getInt8PtrTy ();
   addr = b.CreateBitCast (addr, t);

   // compute the size of the value stored, according to the DataLayaout
   v = i.getValueOperand ();
   size = m->getDataLayout().getTypeStoreSize (v->getType());

   // make sure size has not a crazy value
   switch (size)
   {
   case 1 :
   case 2 :
   case 4 :
   case 8 :
      break;
   default :
      if (size & 7)
      {
         int ssize = (size + 8) & -8;
         std::string s;
         print_value (&i, s);
         DEBUG ("stid: instrumenter: WARNING: casting %u bit store to %u bit store:",
               8 * size, 8 * ssize);
         size = ssize;
         DEBUG ("stid: instrumenter:  %s", s.c_str());
      }
   }

   // instruction to call to the runtime
   b.CreateCall (store_pre, {addr, b.getInt32 (size)});

   // reattach the builder after the store instruction and instrument a second
   // call
   b.SetInsertPoint (i.getNextNode ());
   b.CreateCall (store_post, {addr, b.getInt32 (size)});

   count++;
}

void Instrumenter::visitAllocaInst (llvm::AllocaInst &i)
{
   llvm::IRBuilder<> b (i.getNextNode ());
   llvm::Value *addr;
   llvm::Value *size;
   uint32_t ts;

   //llvm::outs() << "stid: " << i << "\n";

   // get the target size of the allocated type (truncate to 32 bits)
   ts = m->getDataLayout().getTypeStoreSize (i.getAllocatedType());
   // s = m->getDataLayout().getTypeAllocSize (i.getAllocatedType()); // exclude pad space
   
   // multiply type size times number of elements in the array, in 32bits
   size = b.CreateMul (b.getInt32 (ts),
         b.CreateZExtOrTrunc (i.getArraySize(), b.getInt32Ty(), "imnt"), "imnt");
   addr = b.CreateBitCast (&i, b.getInt8PtrTy(), "imnt"); // address space ?
   b.CreateCall (allo, {addr, size});
   count++;
}

void Instrumenter::visitCallInst (llvm::CallInst &i)
{
   //llvm::outs() << "stid: instrumenter: " << i << "\n";
   auto it = substmap.find (i.getCalledFunction ());
   if (it == substmap.end ()) return;
   //DEBUG ("stid: instrumenter: applying substitution");
   i.setCalledFunction (it->second);
}

void Instrumenter::visitReturnInst (llvm::ReturnInst &i)
{
   llvm::IRBuilder<> b (&i);
   //llvm::outs() << "stid: instrumenter: " << i << "\n";
   b.CreateCall (ret, b.getInt16 (funids[i.getParent()->getParent()]));
   count++;
}

#if 0
int Instrumenter::get_fun_id (llvm::Function *f)
{
   auto it = funids.find (f);
   if (it != funids.end()) return it->second;
   return funids[f] = next_call_id++;
}
#endif
