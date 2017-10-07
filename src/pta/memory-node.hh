
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
      Alloca,
      Malloc,
      Top,
      Invalid,
      Nullptr
   };

   /// Constructor for types Function, GlobalVariable, Alloca, Malloc
   MemoryNode (Type t, const llvm::Value *v) :
      NodeBase<MemoryNode> (),
      _type (t),
      _value (v)
   {
      ASSERT (t == Type::Function or
         t == Type::GlobalVariable or
         t == Type::Alloca or
         t == Type::Malloc);
   }

   /// Constructor for types Top, Invalid, Nullptr
   MemoryNode (Type t) :
      NodeBase<MemoryNode> (),
      _type (t),
      _value (nullptr)
   {
      ASSERT (t == Type::Top or
         t == Type::Invalid or
         t == Type::Nullptr);
   }

   /// Returns true iff \p n is a memory location (possibly) pointed by this
   bool points (MemoryNode *n) const { return is_succ (n); }
   const llvm::Value *value () const { return _value; }
   Type type () const { return _type; }

   /// Two memory nodes are euqual iff their addresses in memory are equal.
   /// This is because the MemoryGraph, who is the only factory for instances of
   /// this class, enforces this constraint.
   bool operator== (const MemoryNode &other) { return this == &other; }

  void print(llvm::raw_ostream &s, unsigned indent=0, bool withsucc=true) const;
  void dump () const;

private:
   /// The type of this node
   const Type _type;

   /// Every memory node of type Function, GlobalVariable, Alloca, or Malloc
   /// have an associated LLVM value, the reason for this MemorNode to exist
   const llvm::Value *_value;
};

llvm::raw_ostream &operator<< (llvm::raw_ostream &os, MemoryNode::Type t);
llvm::raw_ostream &operator<< (llvm::raw_ostream &os, const MemoryNode &n);

} // pta
} // stid

#endif

