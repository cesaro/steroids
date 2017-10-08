
#ifndef __STID_PTA_STATE_HH_
#define __STID_PTA_STATE_HH_

#include "pta/memory-graph.hh"
#include "pta/pointer-valuation.hh"

namespace stid {
namespace pta {

struct State
{
   MemoryGraph memory;
   PointerValuation valuation;

#if 0
   void clear ()
   {
      mem.clear ();
      val.clear ();
   }
#endif
};

} // pta
} // stid

#endif


