
#ifndef __STID_PTA_POINTERVALUE_HH_
#define __STID_PTA_POINTERVALUE_HH_

#include "pta/node-base.hh"

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
      ASSERT (v->getType()->isPointerType());
   }

   /// Constructs a PointerValuation for a llvm value \p v of type pointer. The
   /// set of pointed memory objects is a singleton containing object \p n.
   PointerValue (const llvm::Value *v, MemoryNode *n) :
      NodeBase<MemoryNode> (),
      _ptr (v)
   {
      ASSERT (v);
      ASSERT (v->getType()->isPointerType());

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

private:
   /// The llvm value (of type pointer) for which we keep here a valuation
   const llvm::Value *_ptr;
};

} // pta
} // stid

#endif

