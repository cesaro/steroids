
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
//#undef DEBUG // exported by ExecutionEngine.h

#include "verbosity.h"
#include "misc.hh"
#include "../rt/rt.h"
#include "instrumenter.hh"

namespace stid {

bool Instrumenter::instrument (llvm::Module &m, Tlsoffsetmap &tlsoffsetmap)
{
   // cleanup
   reset (m);
   if (not find_rt ()) return false;

#if 0
   DEBUG ("saving to before.ll...");
   dump_ll (&m, "before.ll");
#endif

   // compute a map describing how we are supposed to substitute calls to
   // certain functions, as well as a numeric id for every function
   DEBUG ("stid: instrumenter: initializing substmap and funids mappings");
   init_maps ();

   // substitute all uses of thread-local variables by calls to the function
   // __rt_tls_get_var_addr
   DEBUG ("stid: instrumenter: instrumenting TLS access");
   do_tls_variables (tlsoffsetmap);

#if 0
   DEBUG ("saving to before.ll...");
   dump_ll (&m, "after-tls.ll");
#endif
   // instrument every function
   DEBUG ("stid: instrumenter: instrumenting functions...");
   for (auto &f : m)
   {
      if (is_rt_fun (&f) or f.isDeclaration ()) continue;

      // instrument the CALL event at the beginning of the entry block
      llvm::IRBuilder<> b (&f.getEntryBlock ().front());
      b.CreateCall (call, b.getInt16 (funids[&f]));

      // visit all instructions and instrument appropriately
      count = 0;
      visit (f);
   }

#if 0
   DEBUG ("saving to after.ll...");
   dump_ll (&m, "after-instr.ll");
#endif


   // check that we didn't do anything stupid
#ifdef CONFIG_DEBUG
   DEBUG ("stid: instrumenter: verifying module after instrumentation ...");
#if 0
   for (auto &f : m) 
   {
      if (f.isDeclaration()) continue;
      llvm::outs() << " verifying fun " << f.getName() << "\n";
      llvm::verifyFunction (f, &llvm::outs());
   }
#endif
   llvm::verifyModule (m, &llvm::outs());
#endif
   DEBUG ("stid: instrumenter: done");

   return true;
}

bool Instrumenter::find_rt ()
{
   load_pre  = m->getFunction ("__rt_load_pre");
   load_post = m->getFunction ("__rt_load_post");
   store_pre  = m->getFunction ("__rt_store_pre");
   store_post = m->getFunction ("__rt_store_post");

   allo = m->getFunction ("__rt_allo");
   call = m->getFunction ("__rt_call");
   ret  = m->getFunction ("__rt_ret");

   tls_get_var_addr  = m->getFunction ("__rt_tls_get_var_addr");

   return load_pre != nullptr and store_post != nullptr;
}

bool Instrumenter::is_rt_fun (llvm::Function *f)
{
   return \
      f->getName().startswith ("_rt_") or
      f->getName().startswith ("__rt_") or
      f->getName().startswith ("__VERIFIER_");
}

void Instrumenter::reset (llvm::Module &m)
{
   this->m = &m;
   this->ctx = &m.getContext ();

   // clean the funids map
   next_call_id = 0;
   funids.clear ();

   // clear the function substitution map
   substmap_funs.clear ();
   substmap_loads.clear ();
   substmap_stores.clear ();
}

void Instrumenter::init_maps ()
{
   std::string s;
   int i = 0;

   for (auto &f : *m)
   {
      // associate a unique numeric identifier to every function declaration or
      // definition
      funids[&f] = i++;

      // for every function named _rt_*, we get a substitution table
      if (not is_rt_fun (&f)) continue;
      s = f.getName().substr(4);;
      llvm::Function *ff = m->getFunction (s); // stripping the "_rt_" prefix
      if (not ff) continue;
      DEBUG ("stid: instrumenter: init maps: substmap: %s %-20s -> %s",
            ff->isDeclaration() ? "decl" : "fun ",
            ff->getName().str().c_str(),
            f.getName().str().c_str());
      substmap_funs[ff] = &f;
      if (f.getType () != ff->getType ())
      {
         DEBUG ("stid: instrumenter: init maps: substmap: "
               "WARNING: type mismatch between fun and rtfun:");
         s.clear ();
         print_type (ff->getFunctionType(), s);
         DEBUG ("stid: instrumenter: init maps: substmap:  fun  : %s",
               s.c_str());
         s.clear ();
         print_type (f.getFunctionType(), s);
         DEBUG ("stid: instrumenter: init maps: substmap:  rtfun: %s",
               s.c_str());
      }
   }

   // for every "external global" global value, we determine if we have both a
   // _rt_var_{load,store}_VAR function, and if yes we set up the substitution
   // tables
   for (auto &g : m->globals())
   {
      if (not g.hasExternalLinkage()) continue;
      //llvm::outs() << g << "\n";

      llvm::Function *f1 = m->getFunction("__rt_var_load_" + g.getName().str());
      llvm::Function *f2 = m->getFunction ("__rt_var_store_" + g.getName().str());
      if (f1 and f2)
      {
         substmap_loads[&g] = f1;
         substmap_stores[&g] = f2;
         DEBUG ("stid: instrumenter: init maps: substmap: extv %-20s -> "
               "_rt_var_{load,store}_%s",
               g.getName().str().c_str(),
               g.getName().str().c_str());
      }
      if ((f1 and not f2) or (not f1 and f2))
      {
         DEBUG ("stid: instrumenter: init maps: WARNING: "
               "one of _rt_var_{load,store}_%s given but not the other!",
               g.getName().str().c_str());
      }
   }

   // print function ids
#ifdef VERB_LEVEL_DEBUG
   for (auto &p : funids)
   {
      DEBUG ("stid: instrumenter: init maps: funids: id %#5x %s %s",
            p.second,
            p.first->isDeclaration() ? "decl" : "fun ",
            p.first->getName().str().c_str());
   }
#endif
}

bool Instrumenter::do_external_load (llvm::LoadInst &i)
{
   llvm::IRBuilder<> b (&i);
   llvm::CallInst *call;
   llvm::Function *loadfun;

   // if this is a load to a "hooked" external variable, run the substitution
   auto it = substmap_loads.find (i.getPointerOperand());
   if (it == substmap_loads.end()) return false;
   loadfun = it->second;

   call = b.CreateCall(loadfun);
   i.replaceAllUsesWith(call);
   i.eraseFromParent();
   return true;
}

void Instrumenter::visitLoadInst (llvm::LoadInst &i)
{
   llvm::IRBuilder<> b (&i); // we instrument BEFORE the load instruction
   llvm::Value *addr;
   uint64_t size;

   //llvm::outs() << "stid: " << i << "\n";

   // if it is aload to a "hooked" external variable, all happens inside
   if (do_external_load (i)) return;

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

bool Instrumenter::do_external_store (llvm::StoreInst &i)
{
   llvm::IRBuilder<> b (&i);

   // if this is a store to a "hooked" external variable, run the substitution
   auto it = substmap_stores.find (i.getPointerOperand());
   if (it == substmap_stores.end()) return false;

   b.CreateCall(it->second, {i.getValueOperand()});
   i.eraseFromParent();
   return true;
}

void Instrumenter::visitStoreInst (llvm::StoreInst &i)
{
   llvm::IRBuilder<> b (&i); // we instrument BEFORE the store instruction
   llvm::Value *addr;
   llvm::Value *v;
   llvm::Type *t;
   uint64_t size;

   //llvm::outs() << "stid: " << i << "\n";

   // if it is aload to a "hooked" external variable, all happens inside
   if (do_external_store (i)) return;

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
   auto it = substmap_funs.find (i.getCalledFunction ());
   if (it == substmap_funs.end ()) return;
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

llvm::Instruction *find_some_instruction_user (llvm::Value *v)
{
   while (1)
   {
      //llvm::outs() << "v " << *v << "\n";
      auto u = v->use_begin();
      //if (u == v->use_end()) llvm::outs() << "ret null\n";
      if (u == v->use_end()) return nullptr;
      llvm::Instruction *i = llvm::dyn_cast<llvm::Instruction> (u->getUser());
      if (i) return i;
      ASSERT (llvm::isa<llvm::Constant> (u->getUser()));
      v = u->getUser();
   }
}

void Instrumenter::do_tls_variables (Tlsoffsetmap &map)
{
   for (auto &kv : map)
   {
      // find all uses of this global and replace them with calls to
      // __rt_tls_get_var_addr()
      replace_tls_var (kv.first, kv.second);
   }
}

void Instrumenter::replace_tls_var (llvm::GlobalVariable *g, unsigned offset)
{
   //llvm::ConstantInt *offset;
   llvm::Instruction *ip;
   llvm::Instruction *previ;
   llvm::Instruction *i;
   llvm::User *u;
   llvm::ConstantExpr *ce;
   unsigned opn;

   // find all uses of this global
   //llvm::outs() << "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\n";
   //llvm::outs() << "Global: " << *g << ", " << g->getNumUses() << " uses\n";
   //llvm::outs() << "Offset: " << offset << "\n";
   while (1)
   {
      // find some use of this global
      g->removeDeadConstantUsers ();
      auto it = g->use_begin();
      if (it == g->use_end()) return; // no more uses, we are done
      u = it->getUser();
      opn = it->getOperandNo();

      // compute the insertion point and create an instruction builder there
      ip = find_some_instruction_user (g);
      llvm::IRBuilder<> b (ip);
      //llvm::outs() <<  "target: " << *ip << "\n";

      // create a call to the get_addr function, and bitcast it to the
      // global's type; store the new instruction into an array
      i = b.CreateCall (tls_get_var_addr, {b.getInt32 (offset)});
      //llvm::outs() <<  *i << "\n";
      i = llvm::cast<llvm::Instruction> (b.CreateBitCast (i, g->getType()));
      //llvm::outs() << *i << "\n";

      // If the user 'u' can either be an Instruction or a Constant, and in the
      // latter case it should only be a ConstantExpr, as no other Constant
      // could use the global as operand.
      // If 'u' is an Instruction we just need to replace the operand by "i"
      // If not we need to scan the chain of users until we
      // reach (again) instruction "ip". All users that we find on the way need to be
      // ConstantExpr, and **we convert them to instructions** in order to
      // guarantee that the users of an Instruction are other Instructions and
      // not Constants (an IR invariant)
      
      ASSERT (llvm::isa<llvm::Instruction>(u) or llvm::isa<llvm::ConstantExpr>(u));
      while (llvm::isa<llvm::ConstantExpr>(u))
      {
         //llvm::outs() << "user: " << *u << "\n";
         ce = llvm::cast<llvm::ConstantExpr>(u);
         // create a new instruction identic to the ce, substitute its operand
         // "opn" by the instruction i, and update i
         previ = i;
         i = b.Insert (ce->getAsInstruction());
         i->setOperand (opn, previ);
         //llvm::outs() <<  *i << "\n";

         // find out a user of ce in the code, we now need to substitute it's
         // use of ce for a use of i
         auto it2 = ce->use_begin();
         ASSERT (it2 != ce->use_end());
         u = it2->getUser();
         opn = it2->getOperandNo();
      }

      // at this point u is an instruction and we have inserted 2 or more new
      // instructions mimicking the use of the global by this instruction; we
      // now modify u's operand that (indirectly) used our global
      ASSERT (u);
      ASSERT (i);
      ASSERT (u == ip);
      ASSERT (llvm::isa<llvm::Instruction>(u));
      ASSERT (u->getOperand(opn) != i);
      u->setOperand (opn, i);
      //llvm::cast<llvm::Instruction>(u)->eraseFromParent();

      //llvm::outs() << "result:" << *u << "\n";
      //llvm::outs() << "---\n";
   }
}

} // namespace
