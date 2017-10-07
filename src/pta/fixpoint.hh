
#ifndef __STID_PTA_FIXPOINT_HH_
#define __STID_PTA_FIXPOINT_HH_

#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instruction.h"

#include <deque>

#include "pta/state.hh"

namespace stid {
namespace pta {

class Fixpoint
{
public:
   Fixpoint (const llvm::Module &m) :
      m (m),
      _state ()
   {}

   void eval ();

   State &state ()
   {
      return _state;
   }

private:
   typedef std::deque<const llvm::Instruction*> Frontier;
   bool eval_function (const llvm::Function &f);
   bool eval_instruction (const llvm::Instruction *in);
   void fill_frontier_bfs (const llvm::Function &f, Frontier &frontier);

   const llvm::Module &m;
   State _state;
};

} // pta
} // stid

#endif


