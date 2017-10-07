
#ifndef __STID_PTA_POINTERVALUE_HH_
#define __STID_PTA_POINTERVALUE_HH_

#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"

#include "pta/node-base.hh"
#include "pta/memory-node.hh"

namespace stid {
namespace pta {

class PointerValuation;

// This represents the valuation of a pointer in the program. Since we analyze
// LLVM and LLVM IR is SSA, each value of type pointer is assigned only once.
// Yet during the analysis we may assign it more than once, e.g., after a flow
// join in the CFG. Assignments to a pointer corresponds to adding (possibly
// new) pointed objects
class PointerValue : NodeBase<MemoryNode>
{
public :
   /// Constructs a PointerValuation for a llvm value \p v of type pointer. The
   /// set of pointed memory objects is empty.
   PointerValue (const llvm::Value *v) :
      NodeBase<MemoryNode> (),
      _ptr (v)
   {
      ASSERT (v);
      ASSERT (v->getType()->isPointerTy());
   }

   /// Constructs a PointerValuation for a llvm value \p v of type pointer. The
   /// set of pointed memory objects is a singleton containing object \p n.
   PointerValue (const llvm::Value *v, MemoryNode *n) :
      NodeBase<MemoryNode> (),
      _ptr (v)
   {
      ASSERT (v);
      ASSERT (v->getType()->isPointerTy());

      add (n);
   }

   /// Returns true iff \p n is a memory location (possibly) pointed by this
   bool points (MemoryNode *n) const
   {
      return is_succ (n);
   }

   const llvm::Value *ptr ()
   {
      return _ptr;
   }

   /// Two pointer valuations are equal if they point to the same memory objects
   bool operator== (const PointerValue &other) { return succ == other.succ; }

  void print(llvm::raw_ostream &s) const;
  void dump () const;

private:
   /// The llvm value (of type pointer) for which we keep here a valuation
   const llvm::Value *_ptr;
};

llvm::raw_ostream &operator<< (llvm::raw_ostream &os, const PointerValue &v);

} // pta
} // stid

#endif

