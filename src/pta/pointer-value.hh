
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
class PointerValue : public NodeBase<MemoryNode>
{
public :

   /// The llvm value (of type pointer) for which we keep here a valuation
   const llvm::Value *ptr;

   /// Constructs a PointerValuation for a llvm value \p v of type pointer. The
   /// set of pointed memory objects is empty.
   PointerValue (const llvm::Value *v) :
      NodeBase<MemoryNode> (),
      ptr (v)
   {
      ASSERT (v);
      ASSERT (v->getType()->isPointerTy());
   }

   /// Constructs a PointerValuation for a llvm value \p v of type pointer. The
   /// set of pointed memory objects is a singleton containing object \p n.
   PointerValue (const llvm::Value *v, MemoryNode *n) :
      NodeBase<MemoryNode> (),
      ptr (v)
   {
      ASSERT (v);
      ASSERT (v->getType()->isPointerTy());

      add (n);
   }

   /// Adds all memory nodes represented by \p v to our set. Use the inherited
   /// method include (MemoryNode *) to add the "successors" of a MemoryNode.
   /// Use this method to add all MemoryNodes pointed by a PointerValue.
   /// \returns True iff every added node was already a successor of this node.
   bool merge (PointerValue &v)
   {
      bool already = true;
      for (MemoryNode *n : v) already = add (n) && already;
      return already;
   }

   /// Returns true iff \p n is a memory location (possibly) pointed by this
   bool contains (MemoryNode *n) const
   {
      return is_succ (n);
   }

   /// Two pointer valuations are equal if they point to the same memory objects
   bool operator== (const PointerValue &other) { return succ == other.succ; }

   void print(llvm::raw_ostream &s, const std::string &pref = "") const;
   void dump () const;
};

static inline
llvm::raw_ostream &operator<< (llvm::raw_ostream &os, const PointerValue &v)
{
   v.print (os);
   return os;
}

} // pta
} // stid

#endif

