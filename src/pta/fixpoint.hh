
#ifndef __STID_PTA_FIXPOINT_HH_
#define __STID_PTA_FIXPOINT_HH_

#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"

#include <deque>

#include "pta/state.hh"

namespace stid {
namespace pta {

class Fixpoint
{
public:
   Fixpoint (const llvm::Module &m) :
      m (m),
      state ()
   {}

   State &eval ();

private:
   typedef std::deque<const llvm::Instruction*> Frontier;
   void fill_frontier_bfs (const llvm::Function &f, Frontier &frontier);

   bool eval_function (const llvm::Function &f);
   bool eval_instruction (const llvm::Instruction *in);
   bool eval_instruction_alloca (const llvm::AllocaInst *in);
   bool eval_instruction_bitcast (const llvm::Instruction *in);
   bool eval_instruction_call (const llvm::Instruction *in);
   bool eval_instruction_gep (const llvm::Instruction *in);
   bool eval_instruction_inttoptr (const llvm::Instruction *in);
   bool eval_instruction_load (const llvm::LoadInst *in);
   bool eval_instruction_nop (const llvm::Instruction *in);
   bool eval_instruction_phi (const llvm::Instruction *in);
   bool eval_instruction_ret (const llvm::Instruction *in);
   bool eval_instruction_store (const llvm::StoreInst *in);
   bool eval_instruction_va_arg (const llvm::Instruction *in);
   bool eval_instruction_unimplemented (const llvm::Instruction *in);

   const llvm::Module &m;
   State state;
};

} // pta
} // stid

#endif


