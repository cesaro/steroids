
#include "evconf.hh"
#include "verbosity.h"

eventt::eventt (conft &c, unsigned sidx) :
   _tid (0),
   _sidx (sidx),
   _pre_other (nullptr),
   act ({.type = action_typet::THSTART}),
   redbox (),
   vclock (c.num_ths)
{}

eventt::eventt (unsigned sidx, eventt &creat, unsigned p) :
   _tid (p),
   _sidx (sidx),
   _pre_other (&creat),
   act ({.type = action_typet::THSTART}),
   redbox (),
   vclock (creat.vclock)
{
   ASSERT (creat._tid < p);
   ASSERT (vclock[p] == 0); // my clock should be zero so far!

   vclock[p] += 2; // the +1 is for the redbox events
}

eventt::eventt (unsigned sidx, eventt &p) :
   _tid (p._tid),
   _sidx (sidx),
   _pre_other (nullptr),
   act ({.type = action_typet::THSTART}), // redefined later
   redbox (),
   vclock (p.vclock)
{
   vclock[_tid] += 2; // the +1 is the redbox events
}

eventt::eventt (unsigned sidx, eventt &p, eventt &m) :
   _tid (p._tid),
   _sidx (sidx),
   _pre_other (&m),
   act ({.type = action_typet::THSTART}), // redefined later
   redbox (),
   vclock (p.vclock, m.vclock) // must come after constructing _pre_other
{
   vclock[_tid] += 2; // the +1 is the redbox events
}

inline unsigned eventt::idx (const conft &c)
{ 
   return this - &c.events[_tid][0];
}

conft::conft (action_streamt &s) :
   _stream (s)
{
   rt *rt = s.get_rt ();
   num_ths = rt->trace.num_ths;
   num_mutex = rt->trace.num_mutex;

   ASSERT (num_ths > 0);
   events.reserve (num_ths);
}

// For debug purposes only (FOR NOW)
void conft::print ()
{
   // iterate throught the actions
   int i = 0;
   for (auto ac : _stream)
   {
      // for efficiency purposes ac has type "action_stream_itt" rather than "actiont"
      printf ("idx %5d type %2d '%s' addr %#18lx val %#18lx id %#10x\n",
         i,
         ac.type (),
         _rt_ev_to_str ((enum eventtype) ac.type ()),
         ac.addr (),
         ac.val (),
         ac.id ());
      i++;
      if (i >= 200) break;
   }
}


