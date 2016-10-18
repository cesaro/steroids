
#ifndef __ACTION_HH_
#define __ACTION_HH_

#include <memory>
#include <unordered_map>
#include <vector>
#include <cstdint> // uintxx

#include "verbosity.h"
#include "../rt/rt.h"

typedef uint64_t addrt;

enum class action_typet
{
   // loads
   RD8,
   RD16,
   RD32,
   RD64,
   // stores
   WR8,
   WR16,
   WR32,
   WR64,
   // memory management (alloca -> malloc, ret -> free)
   MALLOC,
   FREE,
   // threads
   THCREAT,
   THSTART,
   THEXIT,
   THJOIN,
   // locks
   MTXINIT,
   MTXLOCK,
   MTXUNLK,
};

struct actiont
{
   action_typet type;
   addrt addr;
   uint64_t val;

   void pretty_print ();
};

const char *actiont_type_str (action_typet t);
const char *actiont_type_str (unsigned t);

#endif
