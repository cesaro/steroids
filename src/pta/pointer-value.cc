
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Debug.h"
#include "llvm/IR/Value.h"

#include "pta/pointer-value.hh"

namespace stid {
namespace pta {

void PointerValue::print(llvm::raw_ostream &os, const std::string &prefix) const
{
   ASSERT (ptr);
   os << prefix;
   os << "Pointer '";
   ptr->printAsOperand (os);
   os << "' may point to " << size() << " objects";
   if (size()) os << ":";
   os << "\n";
   for (const MemoryNode *n : succ)
   {
      n->print (os, prefix + " ", false);
   }
}

void PointerValue::dump () const
{
   print (llvm::dbgs ());
}

} // pta
} // stid
