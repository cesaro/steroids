
#ifndef __STID_PTA_STATE_HH_
#define __STID_PTA_STATE_HH_

#include "llvm/IR/Function.h"

#include "pta/memory-graph.hh"
#include "pta/pointer-valuation.hh"

namespace stid {
namespace pta {

struct State
{
   const llvm::Function &root;
   MemoryGraph memory;
   PointerValuation valuation;

   State (const llvm::Function &root) :
      root (root),
      memory (),
      valuation ()
   {}

   void print (llvm::raw_ostream &os) const;
   void print (llvm::raw_ostream &os, const llvm::GlobalVariable &g) const;
   void print (llvm::raw_ostream &os, const llvm::Function &f) const;
   void print (llvm::raw_ostream &os, const llvm::BasicBlock &b) const;
   void print (llvm::raw_ostream &os, const llvm::Instruction &i) const;
   void dump () const;

#if 0
   void clear ()
   {
      mem.clear ();
      val.clear ();
   }
#endif
};

static inline
llvm::raw_ostream &operator<< (llvm::raw_ostream &os, const State &s)
{
   s.print (os);
   return os;
}

} // pta
} // stid

#endif


