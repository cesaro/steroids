
#ifndef __STID_PTA_MEMORYNODE_HH_
#define __STID_PTA_MEMORYNODE_HH_

#include "llvm/IR/Value.h"

#include "pta/node-base.hh"

#include "verbosity.h"

namespace stid {
namespace pta {

class MemoryGraph;

class MemoryNodeSize
{
   // TODO
};

// This class represents an object stored in memory. Such object will have a
// type, describing either its kind (function, global variable) or the nature of
// the instruction that created this object (alloca, malloc), or represents an
// abstract memory object that the analysis reasons about (Top = all possible
// memory objects, Invalid = an invalid memory address, Nullptr = a NULL
// pointer).
class MemoryNode : public NodeBase<MemoryNode>
{
public :

   /// The type of memory objects tracked by this analysis
   enum Type
   {
      Function,
      GlobalVariable,
      StackVariable,
      HeapVariable,
      Top,
      Invalid,
      Nullptr
   };

   /// The type of this node
   const Type type;

   /// Every memory node of type Function, GlobalVariable, StackVariable, or
   /// HeapVariable have an associated LLVM value, the reason for this
   /// MemoryNode to exist
   const llvm::Value *llvm_value;

   /// Constructor for types Function, GlobalVariable, StackVariable,
   /// HeapVariable
   MemoryNode (Type t, const llvm::Value *v) :
      NodeBase<MemoryNode> (),
      type (t),
      llvm_value (v)
   {
      ASSERT (t == Type::Function or
         t == Type::GlobalVariable or
         t == Type::StackVariable or
         t == Type::HeapVariable);
   }

   /// Constructor for types Top, Invalid, Nullptr
   MemoryNode (Type t) :
      NodeBase<MemoryNode> (),
      type (t),
      llvm_value (nullptr)
   {
      ASSERT (t == Type::Top or
         t == Type::Invalid or
         t == Type::Nullptr);
   }

   /// Returns true iff \p n is a memory location (possibly) pointed by this
   /// MemoryNode
   bool pointsto (MemoryNode *n) const { return is_succ (n); }

   /// Adds one memory node to the set of successors in the graph
   /// Overloads NodeBase::add() because we need to silently ignore adding
   /// successors to the nodes of type Invalid or Nullptr
   /// \returns True iff the added node was already there.
   bool add (MemoryNode *n)
   {
      if (type == Type::Nullptr or type == Type::Invalid) return true;
      return NodeBase<MemoryNode>::add (n);
   }

   /// Adds every successor of \p n to the set of successors of this node.
    // Overloads NodeBase::include() because we need to check that we are not
    // adding pointers to the Nullptr or Invalid nodes. The type variable T is
    // expected to be a subclass of MemoryNode.
    // \returns True iff every added node was already a successor of this node.
   bool include (NodeBase<MemoryNode> *n)
   {
      bool already = true;
      if (type == Type::Nullptr or type == Type::Invalid) return true;
      for (MemoryNode *s : *n)
         already = NodeBase<MemoryNode>::add (s) && already;
      return already;
   }

   /// Two memory nodes are euqual iff their addresses in memory are equal.
   /// This is because the MemoryGraph, who is the only factory for instances of
   /// this class, enforces this constraint.
   bool operator== (const MemoryNode &other) { return this == &other; }

   void print(llvm::raw_ostream &s, unsigned indent=0, bool withsucc=true) const;
   void dump () const;
};

llvm::raw_ostream &operator<< (llvm::raw_ostream &os, MemoryNode::Type t);
llvm::raw_ostream &operator<< (llvm::raw_ostream &os, const MemoryNode &n);

} // pta
} // stid

#endif

