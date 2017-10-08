
#include "llvm/Support/Debug.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/CFG.h"

#include <set>
#include <deque>
#include <iterator>

#include "pta/fixpoint.hh"
#include "pta/memory-node.hh"
#include "misc.hh"

namespace stid {
namespace pta {

State &Fixpoint::eval ()
{
   DEBUG ("stid: pta: eval: module: %d globals", m.size());
   for (const llvm::Function &f : m)
   {
      if (f.getName() != "main16") continue;
      eval_function (f);
      break;
   }

   return state;
}

void Fixpoint::fill_frontier_bfs (const llvm::Function &f, Frontier &frontier)
{
   std::deque<const llvm::BasicBlock*> bbfrontier;
   std::set<const llvm::BasicBlock*> visited;
   const llvm::BasicBlock *bb;

   frontier.clear ();
   bbfrontier.push_back (&f.getEntryBlock());
   while (!bbfrontier.empty())
   {
      // pop a BB from the BB frontier and mark it as visited
      bb = bbfrontier.front();
      bbfrontier.pop_front();
      if (visited.find (bb) != visited.end ()) continue;
      visited.insert (bb);

      // insert all instructions from the BB into the frontier
      for (const llvm::Instruction &in : *bb) frontier.push_back (&in);

      // push the unvisited BB successors
      for (const llvm::BasicBlock *nbb : llvm::successors(bb))
         if (visited.find (nbb) == visited.end ())
            bbfrontier.push_back (nbb);
   }

#ifdef CONFIG_DEBUG
   for (const llvm::Instruction *in : frontier)
      llvm::dbgs() << *in << "\n";
#endif
}

bool Fixpoint::eval_function (const llvm::Function &f)
{
   Frontier frontier;
   const llvm::Instruction *in;
   const llvm::Instruction *in2;
   bool subsumed;

   DEBUG ("stid: pta: eval: fun: name '%s', %d BBs",
      f.getName().str().c_str(), f.size());
   //ASSERT (f.arg_begin() == f.arg_end());

   if (f.isDeclaration ()) return false; // arbitrary

   fill_frontier_bfs (f, frontier);
   while (!frontier.empty())
   {
      // pop an instruction from the frontier
      in = frontier.front();
      frontier.pop_front();

      // evaluate it and skip the evaluation of the users if the evaluation
      // result was already subsumed in the state
      subsumed = eval_instruction (in);
      if (subsumed) DEBUG ("stid: pta: eval: fun: subsumed, skiping users");
      if (subsumed) continue;

      // push the users of the instruction to the frontier
      DEBUG ("stid: pta: eval: fun: not subsumed, pushing %d users",
         std::distance (in->uses().begin(), in->uses().end()));
      for (const llvm::Use &use : in->uses())
      {
         in2 = llvm::cast <const llvm::Instruction> (use.getUser());
         DEBUG ("stid: pta: eval: fun: pushing %s", str(in2).c_str());
         frontier.push_back (in2);
      }
   }
   return false; // arbitrayr
}

#if 0
/// Evaluate the instructions in sequence; if the State stops before
/// evaluating the last one, return false, otherwise true
/// \returns True iff the sucessor basic blocks need to be evaluated
bool Fixpoint::eval_bb (llvm::BasicBlock &bb)
{
   bool subsumed;

   for (const llvm::Instruction *in : bb)
   {
      subsumed = eval_instruction (in);
      if (subsumed) return false;
   }
   return true;
}
#endif

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
      return eval_instruction_gep (in);

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
      return eval_instruction_inttoptr (in);

   case llvm::Instruction::BitCast :
      return eval_instruction_bitcast (in);

   case llvm::Instruction::AddrSpaceCast :
      return eval_instruction_unimplemented (in);

   // Other operators...
   case llvm::Instruction::ICmp :
   case llvm::Instruction::FCmp :
      return eval_instruction_nop (in);

   case llvm::Instruction::PHI :
      return eval_instruction_phi (in);

   case llvm::Instruction::Call :
      return eval_instruction_call (in);

   case llvm::Instruction::Select :
      // FIXME need something else?
      return eval_instruction_phi (in);

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

bool Fixpoint::eval_instruction_bitcast (const llvm::Instruction *in)
{
   return false;
}

bool Fixpoint::eval_instruction_call (const llvm::Instruction *in)
{
   return false;
}

bool Fixpoint::eval_instruction_gep (const llvm::Instruction *in)
{
   return false;
}

bool Fixpoint::eval_instruction_inttoptr (const llvm::Instruction *in)
{
   return false;
}

bool Fixpoint::eval_instruction_load (const llvm::LoadInst *in)
{
   return false;
}

bool Fixpoint::eval_instruction_nop (const llvm::Instruction *in)
{
   DEBUG ("stid: pta: eval: instruction: nop");
   return true;
}

bool Fixpoint::eval_instruction_phi (const llvm::Instruction *in)
{
   return false;
}

bool Fixpoint::eval_instruction_ret (const llvm::Instruction *in)
{
   return false;
}

bool Fixpoint::eval_instruction_store (const llvm::StoreInst *in)
{
   const llvm::Value *value;
   const llvm::Value *ptr;
   bool subsumed = true;

   value = in->getValueOperand();
   ptr = in->getPointerOperand();

   breakme ();

   // if the value to store is not of pointer type, this is a nop and there is
   // no need to evaluate the users
   if (! value->getType()->isPointerTy()) DEBUG ("not ptr");
   if (! value->getType()->isPointerTy()) return true;

   PointerValue &ptrval = state.valuation[ptr];
   PointerValue &valueval = state.valuation[value];

   DEBUG ("ptrval before:");
   for (auto *n : ptrval) n->dump ();
   DEBUG ("valueval before:");
   for (auto *n : valueval) n->dump ();

   for (MemoryNode *written_nod : ptrval)
      subsumed = written_nod->include (&valueval) && subsumed;

   DEBUG ("ptrval after:");
   for (auto *n : ptrval) n->dump ();

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
   return false;
}

} // pta
} // stid

