
#ifndef __STID_PTA_POINTERVALUATION_HH_
#define __STID_PTA_POINTERVALUATION_HH_

#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"

#include <map>

#include "pta/pointer-value.hh"

namespace stid {
namespace pta {

// This class represents the valuations of all pointers in the program. It
// stores a map from llvm::Value (of type pointer) to PointerValue, which stores
// the valuation for one single pointer.
class PointerValuation
{
private:
   typedef std::map<const llvm::Value*, PointerValue> Map;
   Map map;

public:
#if 0
   // FIXME: I need to iterate through the stored PointerValue's, not the pairs,
   // as it's already possible to get the pointer to the llvm value from the
   // PointerValue ;)
   typedef Map::iterator interator;
   typedef Map::const_iterator const_interator;

   iterator begin () { return nodes.begin(); }
   iterator end () { return nodes.end(); }

   const_iterator begin () const { return nodes.begin(); }
   const_iterator end () const { return nodes.end(); }
#endif

   // FIXME: should it be const?
   PointerValue &operator[] (const llvm::Value *v)
   {
      auto it = map.find (v);
      if (it == map.end())
      {
         return map.emplace(v, v).first->second;
      }
      return it->second;
   }
};

} // pta
} // stid

#endif
