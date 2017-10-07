
#include "llvm/IR/Value.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Debug.h"

#include "pta/memory-node.hh"

namespace stid {
namespace pta {

llvm::raw_ostream &operator<< (llvm::raw_ostream &os, MemoryNode::Type t)
{
   switch (t)
   {
   case MemoryNode::Type::Function :
      os << "Function";
      break;
   case MemoryNode::Type::GlobalVariable :
      os << "GlobalVariable";
      break;
   case MemoryNode::Type::Alloca :
      os << "Alloca";
      break;
   case MemoryNode::Type::Malloc :
      os << "Malloc";
      break;
   case MemoryNode::Type::Top :
      os << "Top";
      break;
   case MemoryNode::Type::Invalid :
      os << "Invalid";
      break;
   case MemoryNode::Type::Nullptr :
      os << "Nullptr";
      break;
   }
   return os;
}

llvm::raw_ostream &operator<< (llvm::raw_ostream &os, const MemoryNode &n)
{
   n.print (os);
   return os;
}

void MemoryNode::print(llvm::raw_ostream &os, unsigned idt, bool withsucc) const
{
   os.indent(idt) << "Nod " << this;
   os << ": ty " << _type;
   if (_value)
      os << ", val \"" << *_value << "\"";
   os << ", points to " << size() << " objects";
   os << (size() and withsucc ? ":" : "") << "\n";
   if (withsucc)
   {
      for (const MemoryNode *n : succ)
      {
         os.indent(idt) << " -> ";
         n->print (os, idt, false);
      }
   }
}

void MemoryNode::dump () const
{
   print (llvm::dbgs ());
}

} // pta
} // stid
