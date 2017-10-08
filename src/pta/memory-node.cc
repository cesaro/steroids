
#include "llvm/IR/Value.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
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
      os << "Global location";
      break;
   case MemoryNode::Type::StackVariable :
      os << "Stack location";
      break;
   case MemoryNode::Type::HeapVariable :
      os << "Heap location";
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

void MemoryNode::print_oneline (llvm::raw_ostream &os,
   const std::string &pref) const
{
   os << pref << "Loc " << this << ": " << type;
   if (llvm_value)
   {
      if (type == Type::Function)
         os << " '" << llvm_value->getName() << "'";
      else if (type != Type::GlobalVariable)
      {
         auto in = llvm::cast<const llvm::Instruction>(llvm_value);
         os << "," << *llvm_value << " at fun '";
         os << in->getParent()->getParent()->getName() << "'";
      }
      else
         os << ", " << *llvm_value;
   }
   os << "; stores " << size() << " pointers";

   if (type == Type::Invalid) ASSERT (size() == 0);
   if (type == Type::Nullptr) ASSERT (size() == 0);
}

void MemoryNode::print (llvm::raw_ostream &os, const std::string &pref,
   bool showsuc) const
{
   print_oneline (os, pref);
   os << (size() and showsuc ? ":" : "") << "\n";
   if (showsuc)
   {
      for (const MemoryNode *n : succ)
      {
         os << pref << " -> ";
         n->print_oneline (os, "");
         os << "\n";
      }
   }
}

void MemoryNode::dump () const
{
   print (llvm::dbgs ());
}

} // pta
} // stid
