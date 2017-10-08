
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Debug.h"
#include "llvm/IR/Value.h"

#include <map>

#include "pta/pointer-valuation.hh"

namespace stid {
namespace pta {

void PointerValuation::print (llvm::raw_ostream &os) const
{
   os << "== begin pointer valuation ==\n";
   os << "* " << map.size() << " pointers\n";
   for (const Map::value_type &kv : map) kv.second.print (os);
   os << "== end pointer valuation ==\n";
}

void PointerValuation::dump () const
{
   print (llvm::dbgs ());
}

} // pta
} // stid
