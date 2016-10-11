
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

bool Instrumenter::find_rt ()
{
   ld8  = m->getFunction ("_rt_load8");
   ld16 = m->getFunction ("_rt_load16");
   ld32 = m->getFunction ("_rt_load32");
   ld64 = m->getFunction ("_rt_load64");

   st8  = m->getFunction ("_rt_store8");
   st16 = m->getFunction ("_rt_store16");
   st32 = m->getFunction ("_rt_store32");
   st64 = m->getFunction ("_rt_store64");

   allo = m->getFunction ("_rt_allo");
   mllo = m->getFunction ("_rt_mllo");
   rllo = m->getFunction ("_rt_rllo");
   free = m->getFunction ("_rt_fre");
   call = m->getFunction ("_rt_call");
   ret  = m->getFunction ("_rt_ret");

   return ld32 != nullptr and st32 != nullptr; // for instance
}

bool Instrumenter::is_rt_fun (llvm::Function *f)
{
   return
         f->getName().startswith ("_rt_") or
         f->getName().equals ("free") or
         f->getName().equals ("malloc") or
         f->getName().equals ("realloc") or
         f->getName().equals ("calloc");
}

void Instrumenter::reset (llvm::Module &m)
{
   this->m = &m;
   this->ctx = &m.getContext ();

   // clean the funids map
   next_call_id = 0;
   funids.clear ();
}

bool Instrumenter::instrument (llvm::Module &m)
{
   // cleanup
   reset (m);
   if (not find_rt ()) return false;

   // instrument every function
   for (auto &f : m)
   {
      if (is_rt_fun (&f) or f.isDeclaration ()) continue;

      llvm::outs() << "stid: instrumenting function '" << f.getName() << "'\n";

      // instrument the CALL event at the beginning of the entry block
      llvm::IRBuilder<> b (&f.getEntryBlock ().front());
      b.CreateCall (call, b.getInt16 (get_fun_id (&f)));

      // visit all instructions and instrument appropriately
      count = 0;
      visit (f);
      llvm::outs() << "stid: done, " << count << " instructions instrumented.\n";

      for (auto i : f.users()) llvm::outs() << "stid:  used: " << *i << "\n";

   }

   // print function ids
   DEBUG ("stid: instrumentation: function map:");
   for (auto &p : funids)
   {
      DEBUG ("stid: instrumentation:   id %4d fun %s",
            p.second, p.first->getName().str().c_str());
   }

   // check that we didn't do anything stupid
   DEBUG ("stid: verifying module after instrumentation ...");
   llvm::verifyModule (m, &llvm::outs());
   DEBUG ("stid: done");

   return true;
}

#if 0
void Instrumenter::visitLoadInst (llvm::LoadInst &i)
{
   llvm::IRBuilder<> b (i.getNextNode ()); // we instrument AFTER the load instruction
   //llvm::IRBuilder<> b (&i);
   llvm::Value *addr;
   llvm::Value *v;
   llvm::Type *t;
   llvm::Function *f;
   llvm::Value *newi;

   //static int count = 0;
   //count++;
   //if (count >= 3) return;
   //llvm::outs() << "stid: " << i << "\n";

   // if we are tring to load a pointer, make a bitcast to uint64_t
   addr = i.getPointerOperand ();
   v = &i;
   if (i.getType()->isPointerTy())
   {
      // t = i64*, address is bitcasted to type t
      t = llvm::Type::getInt64PtrTy (*ctx, addr->getType()->getPointerAddressSpace());
      addr = b.CreateBitCast (addr, t, "imnt");
      // lodaded value is converted to i64
      v = b.CreatePtrToInt (&i, b.getInt64Ty(), "imnt");
   }

   // check if we support the bitwith
   // use isSized() + queries to the DataLayaout system to generalize this
   if (not v->getType()->isIntegerTy())
      throw std::runtime_error ("Instrumentation: load: non-integer type");
   switch (v->getType()->getIntegerBitWidth ())
   {
   case 8 : f = ld8; break;
   case 16 : f = ld16; break;
   case 32 : f = ld32; break;
   case 64 : f = ld64; break;
   default :
      throw std::runtime_error ("Instrumentation: load: cannot handle bitwith");
   }

   // instruction to call to the runtime
   newi = b.CreateCall (f, {addr, v});

   // 

   count++;
}
#endif

void Instrumenter::visitLoadInst (llvm::LoadInst &i)
{
   llvm::IRBuilder<> b (i.getNextNode ());
   llvm::Value *addr;
   llvm::Type *t;
   llvm::Function *f;
   llvm::Value *newi;

   //static int count = 0;
   //count++;
   //if (count >= 3) return;
   //llvm::outs() << "stid: visit load\n";
   //llvm::outs() << "stid:   i" << i << "\n";

   // if we are tring to load a pointer, make a bitcast to uint64_t
   addr = i.getPointerOperand ();
   if (i.getType()->isPointerTy())
   {
      // t = i64*, address is bitcasted to type t
      t = llvm::Type::getInt64PtrTy (*ctx, addr->getType()->getPointerAddressSpace());
      addr = b.CreateBitCast (addr, t, "imnt");
      f = ld64;
   }
   else
   {
      // check if we support the bitwith
      // use isSized() + queries to the DataLayaout system to generalize this
      if (not i.getType()->isIntegerTy())
         throw std::runtime_error ("Instrumentation: load: non-integer type");
      switch (i.getType()->getIntegerBitWidth ())
      {
      case 8 : f = ld8; break;
      case 16 : f = ld16; break;
      case 32 : f = ld32; break;
      case 64 : f = ld64; break;
      default :
         throw std::runtime_error ("Instrumentation: load: cannot handle bitwith");
      }
   }

   // instruction to call to the runtime
   newi = b.CreateCall (f, {addr});

   // if we typecasted a pointer to a pointer, then undo the cast after loading
   if (i.getType()->isPointerTy())
   {
      newi = b.CreateIntToPtr (newi, i.getType(), "imnt");
   }
   //llvm::outs() << "stid:   newi " << *newi << "\n";

   // replace all uses
   i.replaceAllUsesWith (newi);

#if 0
   // this won't work, as you invalidate the iterator on the first modification
   // and the loop iterates only 1 time
   llvm::outs() << "stid:   i.uses:\n";
   for (auto &use : i.uses())
   {
      llvm::User *user = use.getUser ();
      llvm::outs() << "stid:     user " << user << " operand " <<
            use.getOperandNo () << " dump " << *user << "\n";
      //user->setOperand (use.getOperandNo (), newi);
   }
#endif

   // remove i
   i.eraseFromParent ();

   count++;
}

void Instrumenter::visitStoreInst (llvm::StoreInst &i)
{
   llvm::IRBuilder<> b (&i); // we instrument BEFORE the store instruction
   llvm::Value *addr;
   llvm::Value *v;
   llvm::Type *t;
   llvm::Function *f;

   //static int count = 0;
   //count++;
   //if (count >= 3) return;
   //llvm::outs() << "stid: " << i << "\n";

   // if we are tring to store a pointer, make a bitcast to uint64_t
   v = i.getValueOperand ();
   addr = i.getPointerOperand ();
   if (v->getType()->isPointerTy())
   {
      // t = i64*, address is bitcasted to type t
      t = llvm::Type::getInt64PtrTy (*ctx, addr->getType()->getPointerAddressSpace());
      addr = b.CreateBitCast (addr, t, "imnt");
      // lodaded value is converted to i64
      v = b.CreatePtrToInt (v, b.getInt64Ty(), "imnt");
   }

   // check if we support the bitwith
   if (not v->getType()->isIntegerTy())
      throw std::runtime_error ("Instrumentation: store: non-integer type");
   switch (v->getType()->getIntegerBitWidth ())
   {
   case 8 : f = st8; break;
   case 16 : f = st16; break;
   case 32 : f = st32; break;
   case 64 : f = st64; break;
   default :
      throw std::runtime_error ("Instrumentation: store: cannot handle bitwith");
   }

   // instruction to call to the runtime
   b.CreateCall (f, {addr, v});
   count++;
}

void Instrumenter::visitAllocaInst (llvm::AllocaInst &i)
{
   llvm::IRBuilder<> b (i.getNextNode ());
   llvm::Value *addr;
   llvm::Value *size;
   uint32_t ts;

   //static int count = 0;
   //count++;
   //if (count >= 3) return;
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

//void Instrumenter::visitCallInst (llvm::CallInst &i) { }

void Instrumenter::visitReturnInst (llvm::ReturnInst &i)
{
   llvm::IRBuilder<> b (&i);
   llvm::outs() << "stid: " << i << "\n";
   b.CreateCall (ret, b.getInt16 (get_fun_id (i.getParent()->getParent())));
   count++;
}

int Instrumenter::get_fun_id (llvm::Function *f)
{
   auto it = funids.find (f);
   if (it != funids.end()) return it->second;
   return funids[f] = next_call_id++;
}
