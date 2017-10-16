
#ifndef __STID_REPLAY_HH_
#define __STID_REPLAY_HH_

#include <vector>

#include "../../rt/rt.h"

namespace stid {


class Replay : public std::vector<struct replayevent>
{
public:
   typedef std::vector<struct replayevent> Vector;
};

} // namespace

#endif
