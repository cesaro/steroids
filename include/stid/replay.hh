
#ifndef __STID_REPLAY_HH_
#define __STID_REPLAY_HH_

#include "../../rt/rt.h"

namespace stid {

class Replay : public std::vector<struct replayevent>
{
};

} // namespace

#endif