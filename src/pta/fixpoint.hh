
#ifndef __STID_PTA_FIXPOINT_HH_
#define __STID_PTA_FIXPOINT_HH_

#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"

#include <deque>

#include "pta/state.hh"
#include "pta/worklist.hh"

namespace stid {
namespace pta {

class Fixpoint
{
public:
   Fixpoint (const llvm::Function &root) :
      m (*root.getParent()),
      root (root),
      state (root)
   {}

   State &run ();

private:
   typedef Worklist<const llvm::Instruction*> Frontier;
   void fill_frontier_bfs (Frontier &frontier, const llvm::Instruction &i);
   void fill_frontier_bfs (Frontier &frontier, const llvm::Function &f);

   bool eval_function (const llvm::Function &f);
   bool eval_function_allinstr (const llvm::Function &f, Frontier &front);
   bool eval_instruction (const llvm::Instruction *in);
   bool eval_instruction_alloca (const llvm::AllocaInst *in);
   bool eval_instruction_bitcast (const llvm::BitCastInst *in);
   bool eval_instruction_call (const llvm::Instruction *in);
   bool eval_instruction_gep (const llvm::GetElementPtrInst *in);
   bool eval_instruction_inttoptr (const llvm::IntToPtrInst *in);
   bool eval_instruction_load (const llvm::LoadInst *in);
   bool eval_instruction_nop (const llvm::Instruction *in);
   bool eval_instruction_phi (const llvm::PHINode *in);
   bool eval_instruction_select (const llvm::SelectInst *in);
   bool eval_instruction_ret (const llvm::Instruction *in);
   bool eval_instruction_store (const llvm::StoreInst *in);
   bool eval_instruction_va_arg (const llvm::Instruction *in);
   bool eval_instruction_unimplemented (const llvm::Instruction *in);

   void eval_constant (const llvm::Constant *c);

   const llvm::Module &m;
   const llvm::Function &root;
   State state;
};

} // pta
} // stid

#endif
