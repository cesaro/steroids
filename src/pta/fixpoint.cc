
#include "llvm/Support/Debug.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/CFG.h"

#include <set>
#include <deque>
#include <iterator>

#include "pta/fixpoint.hh"
#include "pta/worklist.hh"
#include "pta/memory-node.hh"
#include "misc.hh"
#include "verbosity.h"

namespace stid {
namespace pta {

void Fixpoint::fill_frontier_bfs(Frontier &frontier, const llvm::Instruction &in)
{
   unsigned opn, i;
   const llvm::Value *v;

   // In llvm, instruction operands that are llvm::Constant will not be visited
   // as we walk through the BBs. Those operands can contain either simple
   // references to memory (null, @function, @global) or complex
   // llvm::ConstantExpr, which devishly mimic instruction set but using not the
   // class llvm::Instruction. We could push the constants to the frontier, so
   // that they are visited in order, but we are going to visit them here
   // instead, it's easier. We can do this because constants cannot change their
   // value during the execution, so the evaluation we make here is final and
   // sound (overapproximates their value).
   opn = in.getNumOperands();
   for (i = 0; i < opn; i++)
   {
      v = in.getOperand(i);
      if (auto c = llvm::dyn_cast<llvm::Constant>(v))
      {
         if (! c->getType()->isPointerTy()) continue;
         eval_constant (c);
      }
   }

   // push the instruction to the frontier
   frontier.push (&in);
}

void Fixpoint::fill_frontier_bfs (Frontier &frontier, const llvm::Function &f)
{
   Worklist<const llvm::BasicBlock*> bblist;
   std::set<const llvm::BasicBlock*> visited;
   const llvm::BasicBlock *bb;

   ASSERT (frontier.empty());
   ASSERT (! f.isDeclaration());

   bblist.push (&f.getEntryBlock());
   while (!bblist.empty())
   {
      // pop a BB from the BB frontier and mark it as visited
      bb = bblist.pop();
      DEBUG ("stid: pta: eval: fun: bbs: pop %s", str2(bb).c_str());
      if (visited.find (bb) != visited.end ()) continue;
      visited.insert (bb);

      // insert all instructions from the BB into the frontier
      for (const llvm::Instruction &in : *bb)
         fill_frontier_bfs (frontier, in);

      // push the unvisited BB successors
      for (const llvm::BasicBlock *nbb : llvm::successors(bb))
         if (visited.find (nbb) == visited.end ())
         {
            bool b = bblist.push (nbb);
            DEBUG ("stid: pta: eval: fun: bbs: push %s: %s",
               str2(nbb).c_str(), b ? "added" : "skipped");
         }
   }

#ifdef CONFIG_DEBUG
   for (const llvm::Instruction *in : frontier)
      DEBUG ("stid: pta: eval: fun: bbs: frontier: %s", str(in).c_str());
#endif
}

State &Fixpoint::run ()
{
   DEBUG ("stid: pta: eval: module: %d globals", m.size());

   eval_function (root);

   return state;
}

bool Fixpoint::eval_function (const llvm::Function &f)
{
   Frontier frontier;
   bool subsumed;

   DEBUG ("stid: pta: eval: fun: name '%s', %d BBs",
      f.getName().str().c_str(), f.size());

   // ensure the memory graph has a node for f and stat.evaluation[f] is set up
   eval_constant (&f);

   // skip declarations
   if (f.isDeclaration ()) return false; // FIXME: ret value

   // Instead of doing an advanced algorithm we just push all instructions to a
   // work list and walk through all of them again and again until we reach
   // fixpoint for this function. Instructions are not removed from the
   // frontier, despite its name, we use it as a vector of all instructions of
   // the function.
   fill_frontier_bfs (frontier, f);
   while (1)
   {
      subsumed = eval_function_allinstr (f, frontier);
      if (subsumed) break;
   }

   return true; // FIXME
}

bool Fixpoint::eval_function_allinstr (const llvm::Function &f, Frontier &front)
{
   bool subsumed = true;

   DEBUG ("stid: pta: eval: fun-easy: ==========================");
   DEBUG ("stid: pta: eval: fun-easy: evaluating %zd instructions", front.size());

   for (const llvm::Instruction *in : front)
   {
      subsumed = eval_instruction (in) && subsumed;
      if (!subsumed)
         DEBUG ("stid: pta: eval: fun-easy: NOT SUBSUMED!");
   }

   return subsumed;
}

/// \returns True iff the evaluation of the instruction could possibly update
/// the state but it didn't because all produced changes were already subsumed
bool Fixpoint::eval_instruction (const llvm::Instruction *in)
{
   DEBUG ("stid: pta: eval: instruction: =========");
   DEBUG ("stid: pta: eval: instruction: %s", str(in).c_str());

   switch (in->getOpcode())
   {
   // Terminator Instructions
   case llvm::Instruction::Ret :
      return eval_instruction_ret (in);

   case llvm::Instruction::Br :
   case llvm::Instruction::Switch :
   case llvm::Instruction::IndirectBr :
   case llvm::Instruction::Invoke :
   case llvm::Instruction::Resume :
   case llvm::Instruction::Unreachable :
   //catchswitch       llvm v6
   //catchret          llvm v6
   //cleanupret        llvm v6
      return eval_instruction_nop (in);

   // Standard binary operators...
   case llvm::Instruction::Add :
   case llvm::Instruction::FAdd :
   case llvm::Instruction::Sub :
   case llvm::Instruction::FSub :
   case llvm::Instruction::Mul :
   case llvm::Instruction::FMul :
   case llvm::Instruction::UDiv :
   case llvm::Instruction::SDiv :
   case llvm::Instruction::FDiv :
   case llvm::Instruction::URem :
   case llvm::Instruction::SRem :
   case llvm::Instruction::FRem :
      return eval_instruction_nop (in);

   // Logical operators (integer operands)
   case llvm::Instruction::Shl :
   case llvm::Instruction::LShr :
   case llvm::Instruction::AShr :
   case llvm::Instruction::And :
   case llvm::Instruction::Or :
   case llvm::Instruction::Xor :
      return eval_instruction_nop (in);

   // Memory operators...
   case llvm::Instruction::Alloca :
      return eval_instruction_alloca (llvm::cast<llvm::AllocaInst>(in));

   case llvm::Instruction::Load :
      return eval_instruction_load (llvm::cast<llvm::LoadInst>(in));

   case llvm::Instruction::Store :
      return eval_instruction_store (llvm::cast<llvm::StoreInst>(in));

   case llvm::Instruction::GetElementPtr :
      return eval_instruction_gep (llvm::cast<llvm::GetElementPtrInst>(in));

   case llvm::Instruction::Fence :
      return eval_instruction_nop (in);

   case llvm::Instruction::AtomicCmpXchg :
   case llvm::Instruction::AtomicRMW :
      return eval_instruction_unimplemented (in);

   // Cast operators ...
   case llvm::Instruction::Trunc :
   case llvm::Instruction::ZExt :
   case llvm::Instruction::SExt :
   case llvm::Instruction::FPToUI :
   case llvm::Instruction::FPToSI :
   case llvm::Instruction::UIToFP :
   case llvm::Instruction::SIToFP :
   case llvm::Instruction::FPTrunc :
   case llvm::Instruction::FPExt :
   case llvm::Instruction::PtrToInt :
      return eval_instruction_nop (in);

   case llvm::Instruction::IntToPtr :
      return eval_instruction_inttoptr (llvm::cast<llvm::IntToPtrInst>(in));

   case llvm::Instruction::BitCast :
      return eval_instruction_bitcast (llvm::cast<llvm::BitCastInst>(in));

   case llvm::Instruction::AddrSpaceCast :
      return eval_instruction_unimplemented (in);

   // Other operators...
   case llvm::Instruction::ICmp :
   case llvm::Instruction::FCmp :
      return eval_instruction_nop (in);

   case llvm::Instruction::PHI :
      return eval_instruction_phi (llvm::cast<llvm::PHINode>(in));

   case llvm::Instruction::Call :
      return eval_instruction_call (in);

   case llvm::Instruction::Select :
      // FIXME need something else?
      return eval_instruction_select (llvm::cast<llvm::SelectInst>(in));

   case llvm::Instruction::VAArg :
      return eval_instruction_va_arg (in);

   case llvm::Instruction::ExtractElement :
   case llvm::Instruction::InsertElement :
   case llvm::Instruction::ShuffleVector :
   case llvm::Instruction::ExtractValue :
   case llvm::Instruction::InsertValue :
   case llvm::Instruction::LandingPad :
   //catchpad          llvm v6
   //cleanuppad        llvm v6
      return eval_instruction_unimplemented (in);

   default :
      DEBUG ("stid: pta: eval: instruction: default case!");
      return eval_instruction_unimplemented (in);
   }

   // unreachable
   ASSERT (0);
}

bool Fixpoint::eval_instruction_alloca (const llvm::AllocaInst *in)
{
   bool subsumed;
   MemoryNode *n = state.memory[in];
   PointerValue &val = state.valuation[in];

   ASSERT (n->llvm_value == in);
   if (in->getAllocatedType()->isPointerTy())
      ASSERT (n->pointsto(state.memory.invalid()));
   else
      ASSERT (n->empty());
   ASSERT (val.ptr == in);
   ASSERT (val.size() <= 1); // empty set or singleton {n}

   // add the Alloca memory object to the set (singleton) of pointers
   subsumed = val.add (n);
   return subsumed;
}

bool Fixpoint::eval_instruction_bitcast (const llvm::BitCastInst *in)
{
   bool subsumed;

   // if the result type is not a pointer, this is a nop
   if (! in->getType()->isPointerTy()) return true;

   ASSERT (in->getType() == in->getDestTy());

   // if the result type is a pointer but the original type is not, then we add
   // Top; the evaluation was is subsumed iff Top was already (in) the set of
   // values for the pointer
   if (! in->getSrcTy()->isPointerTy())
   {
      PointerValue &val = state.valuation[in];
      subsumed = val.contains (state.memory.top());
      val.clear ();
      val.add (state.memory.top());
      return subsumed;
   }

   // both the source and destination types are pointer types, merge the value
   // of the source pointer to the destination pointer
   return state.valuation[in].merge (state.valuation[in->getOperand(0)]);
}

bool Fixpoint::eval_instruction_call (const llvm::Instruction *in)
{
   ASSERT (0);
   return true;
}

bool Fixpoint::eval_instruction_gep (const llvm::GetElementPtrInst *in)
{
   ASSERT (in->getPointerOperand()->getType()->isPointerTy());
   // a GEP instruction returns a pointer within the same memory block as the
   // argument it takes
   return state.valuation[in].merge (state.valuation[in->getPointerOperand()]);
}

bool Fixpoint::eval_instruction_inttoptr (const llvm::IntToPtrInst *in)
{
   // conversion from integer to pointer yields a valid unknown memory location
   return state.valuation[in].add (state.memory.top());
}

bool Fixpoint::eval_instruction_load (const llvm::LoadInst *in)
{
   const llvm::Value *ptr;
   bool subsumed = true;

   ptr = in->getPointerOperand();

   // if the loaded value is not pointer-typed, this is a nop and there is no
   // need to evaluate the users
   if (! in->getType()->isPointerTy()) DEBUG ("not ptr");
   if (! in->getType()->isPointerTy()) return true;

   PointerValue &ptrval = state.valuation[ptr];
   PointerValue &retval = state.valuation[in];

   DEBUG ("ptrval before:");
   for (auto *n : ptrval) n->dump ();
   DEBUG ("retval before:");
   for (auto *n : retval) n->dump ();

   for (MemoryNode *read_nod : ptrval)
      subsumed = retval.include (read_nod) && subsumed;

   DEBUG ("retval after:");
   for (auto *n : retval) n->dump ();

   return subsumed;
}

bool Fixpoint::eval_instruction_nop (const llvm::Instruction *in)
{
   DEBUG ("stid: pta: eval: instruction: nop");
   return true;
}

bool Fixpoint::eval_instruction_phi (const llvm::PHINode *in)
{
   unsigned count, i;
   bool subs = true;

   // if the loaded value is not pointer-typed, this is a nop and there is no
   // need to evaluate the users
   if (! in->getType()->isPointerTy()) DEBUG ("not ptr");
   if (! in->getType()->isPointerTy()) return true;

   PointerValue &dst = state.valuation[in];
   DEBUG ("dst before:");
   for (auto *n : dst) n->dump ();

   count = in->getNumIncomingValues();
   for (i = 0; i < count; ++i)
      subs = dst.merge (state.valuation[in->getIncomingValue(i)]) && subs;

   DEBUG ("dst after:");
   for (auto *n : dst) n->dump ();

   return subs;
}

bool Fixpoint::eval_instruction_select (const llvm::SelectInst *in)
{
   bool subsumed = true;

   // if the loaded value is not pointer-typed, this is a nop and there is no
   // need to evaluate the users
   if (! in->getType()->isPointerTy()) DEBUG ("not ptr");
   if (! in->getType()->isPointerTy()) return true;

   PointerValue &dst = state.valuation[in];
   DEBUG ("dst before:");
   for (auto *n : dst) n->dump ();

   subsumed = dst.merge (state.valuation[in->getTrueValue()]) && subsumed;
   subsumed = dst.merge (state.valuation[in->getFalseValue()]) && subsumed;

   DEBUG ("dst after:");
   for (auto *n : dst) n->dump ();

   return subsumed;
}

bool Fixpoint::eval_instruction_ret (const llvm::Instruction *in)
{
   // FIXME: implement
   return true;
}

bool Fixpoint::eval_instruction_store (const llvm::StoreInst *in)
{
   const llvm::Value *value;
   const llvm::Value *ptr;
   bool subsumed = true;

   value = in->getValueOperand();
   ptr = in->getPointerOperand();

   // if the value to store is not of pointer type, this is a nop and there is
   // no need to evaluate the users
   if (! value->getType()->isPointerTy()) DEBUG ("not ptr");
   if (! value->getType()->isPointerTy()) return true;

   PointerValue &ptrval = state.valuation[ptr];
   PointerValue &retval = state.valuation[value];

   //DEBUG ("ptrval before:");
   //for (auto *n : ptrval) n->dump ();
   //DEBUG ("retval before:");
   //for (auto *n : retval) n->dump ();

   for (MemoryNode *written_nod : ptrval)
      subsumed = written_nod->include (&retval) && subsumed;

   //DEBUG ("ptrval after:");
   //for (auto *n : ptrval) n->dump ();

   return subsumed;
}

bool Fixpoint::eval_instruction_unimplemented (const llvm::Instruction *in)
{
   DEBUG ("stid: pta: eval: instruction: unimplemented");
   ASSERT (0);
   return false;
}

bool Fixpoint::eval_instruction_va_arg (const llvm::Instruction *in)
{
   ASSERT (0);
   return true;
}

void Fixpoint::eval_constant (const llvm::Constant *c)
{
   MemoryNode *n;

   // To evaluate a constant we need to first evaluate the operands of the
   // constant and then the constant itself. Currently no supported constant has
   // operands, so this function is not recursive. Otherwise it should be.

   // null ptr
   // undef value (address)
   // functions
   // global variables
   // constant agregates (structs, arrays, vectors)
   // constant expressions

   DEBUG ("stid: pta: eval: const: %s", str2(c).c_str());

   if (llvm::isa<llvm::GlobalVariable>(c))
   {
      // add a new GlobalVariable node and set up c's valuation
      n = state.memory[c];
      state.valuation[c].add (n);
      ASSERT (state.valuation[c].size() == 1);
   }
   else if (llvm::isa<llvm::Function>(c))
   {
      // add a new Function node and set up c's valuation
      n = state.memory[c];
      state.valuation[c].add (n);
      ASSERT (state.valuation[c].size() == 1);
   }
   else if (llvm::isa<llvm::ConstantPointerNull>(c))
   {
      // set up c's valuation
      state.valuation[c].add (state.memory.nullptr_());
      ASSERT (state.valuation[c].size() == 1);
   }
   else if (llvm::isa<llvm::UndefValue>(c))
   {
      // set up c's valuation
      state.valuation[c].add (state.memory.invalid());
      ASSERT (state.valuation[c].size() == 1);
   }
   else if (auto e = llvm::dyn_cast<llvm::ConstantExpr>(c))
   {
      (void) e;
      //  recursion here
      ASSERT (0 && "Constant expression!!");
   }
   else if (auto g = llvm::dyn_cast<llvm::GlobalAlias>(c))
   {
      (void) g;
      // in llvm 6 GlobalAlias and a new class GlobalIFunc inherit from an other
      // intermediate one
      ASSERT (0 && "Global Alias!!");
   }
   else
   {
      ASSERT (0 && "Unsupported constant!!");
   }
}

} // pta
} // stid

