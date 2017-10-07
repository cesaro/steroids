
#include "llvm/IR/Function.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/CFG.h"

#include <set>
#include <deque>

#include "pta/fixpoint.hh"

namespace stid {
namespace pta {

void Fixpoint::eval ()
{
   for (const llvm::Function &f : m)
   {
      eval_function (f);
      break;
   }
}

void Fixpoint::fill_frontier_bfs (const llvm::Function &f, Frontier &frontier)
{
   std::deque<const llvm::BasicBlock*> bbfrontier;
   std::set<const llvm::BasicBlock*> visited;
   const llvm::BasicBlock *bb;

   frontier.clear ();
   bbfrontier.push_back (&f.getEntryBlock());
   while (!frontier.empty())
   {
      // pop a BB from the BB frontier and mark it as visited
      bb = bbfrontier.front();
      bbfrontier.pop_front();
      ASSERT (visited.find (bb) == visited.end ());
      visited.insert (bb);

      // insert all instructions from the BB into the frontier
      for (const llvm::Instruction &in : *bb) frontier.push_back (&in);

      // push the unvisited BB successors
      for (const llvm::BasicBlock *nbb : llvm::successors(bb))
         if (visited.find (nbb) != visited.end ())
            bbfrontier.push_back (nbb);
   }
}

bool Fixpoint::eval_function (const llvm::Function &f)
{
   Frontier frontier;
   const llvm::Instruction *in;
   const llvm::Instruction *in2;
   bool subsumed;

   ASSERT (f.arg_begin() == f.arg_end());

   llvm_unreachable ("error!");
   
   fill_frontier_bfs (f, frontier);
   while (!frontier.empty())
   {
      // pop an instruction from the frontier
      in = frontier.front();
      frontier.pop_front();

      // evaluate it and skip the evaluation of the users if the evaluation
      // result was already subsumed in the state
      subsumed = eval_instruction (in);
      if (subsumed) continue;

      // push the users of the instruction to the frontier
      for (const llvm::Use &use : in->uses())
      {
         in2 = llvm::cast <const llvm::Instruction> (use.getUser());
         frontier.push_back (in2);
      }
   }
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
   return true;

   switch (in->getOpcode())
   {
   case llvm::Instruction::Ret :
      break;
   case llvm::Instruction::Br :
   case llvm::Instruction::Switch :
   case llvm::Instruction::IndirectBr :
      return false;
      break;

   case llvm::Instruction::Alloca :
   case llvm::Instruction::Call :
   case llvm::Instruction::Invoke :
   case llvm::Instruction::Load :
   case llvm::Instruction::Store :
   case llvm::Instruction::GetElementPtr :
   case llvm::Instruction::PHI :
   case llvm::Instruction::BitCast :
   case llvm::Instruction::IntToPtr :
   case llvm::Instruction::Select :
   case llvm::Instruction::VAArg :
   case llvm::Instruction::ExtractValue :
   case llvm::Instruction::InsertValue :
   case llvm::Instruction::LandingPad :
   case llvm::Instruction::Resume :
   case llvm::Instruction::AtomicRMW :
   case llvm::Instruction::AtomicCmpXchg :
      break;
   }
}

} // pta
} // stid

