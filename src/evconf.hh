
#ifndef __EVCONF_HH_
#define __EVCONF_HH_

#include <vector>

#include "action.hh"
#include "action_stream.hh"
#include "vclock.hh"

class conft;

class eventt
{
private:
   unsigned _tid;
   unsigned _sidx;
   eventt *_pre_other; // needs to be initialized before vclock

public:
   eventt (conft &c, unsigned sidx); // bottom (THSTART for process 0)
   eventt (unsigned sidx, eventt &creat, unsigned p); // THSTART for process p, creat is the THCREAT
   eventt (unsigned sidx, eventt &p); // one predecessor (process)
   eventt (unsigned sidx, eventt &p, eventt &m); // two predecessors (process, memory/exit)

   actiont act;
   std::vector<actiont> redbox;
   vclockt vclock;

   // thread id
   inline unsigned tid () { return _tid; }

   // index in the stream
   inline unsigned sidx () { return _sidx; }

   // index in the process array
   inline unsigned idx (const conft &c);

   // predecessor in another thread, if any
   inline eventt *pre_other () { return _pre_other; }

   // predecessor in my thread, or null if THSTART
   inline eventt *pre_proc ()
      { return act.type != action_typet::THSTART ? this - 1 : 0; }

   // the bottom even is always the first in the stream
   inline bool is_bottom () { return _sidx == 0; }
};

class conft {
public:
   conft (action_streamt &s); // st should be const 

   void build ();

   void print ();

private:
   bool add_blue_event ();

   std::vector<std::vector<eventt>> events;
   std::unordered_map<addrt,eventt*> mutexmax;
   action_streamt &_stream;
   int num_ths;
   int num_mutex;

   friend class eventt;
};

#endif
