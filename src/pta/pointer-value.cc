
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Debug.h"
#include "llvm/IR/Value.h"

#include "pta/pointer-value.hh"

namespace stid {
namespace pta {

llvm::raw_ostream &operator<< (llvm::raw_ostream &os, const PointerValue &v)
{
   v.print (os);
   return os;
}

void PointerValue::print(llvm::raw_ostream &os) const
{
   ASSERT (ptr);
   os << "Pointer \"" << *ptr << "\" may point to " << size() << " objects:\n";
   for (const MemoryNode *n : succ)
   {
      n->print (os, 2, false);
   }
}

void PointerValue::dump () const
{
   print (llvm::dbgs ());
}

} // pta
} // stid
