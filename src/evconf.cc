
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

void conft::build ()
{
   int sidx = 0;

   // current thread identifier
   int cur_tid = 0;

   // create the bottom event
   eventt ev = eventt(*this, sidx);

   // safety check
   bool next_is_global = false;

   auto st_it = _stream.begin ();
   while (st_it != _stream.end ())
   {
     next_is_global = add_red_events (st_it, sidx, ev);
     // at this point, the red events are already in ev
     // next_is_global ensures that the last event of the
     // stream is a 'global' event
     ASSERT (next_is_global);
   }
}

bool conft::add_red_events (action_stream_itt &it, int &i, eventt &b_ev)
{
   auto act = *it++;

   int type;
   for (auto act : _stream)
   {
      type = act.type ();
      switch (type)
      {
      // loads
      case _RD8       : return true;
      case _RD16      : return true;
      case _RD32      : return true;
      case _RD64      : return true;
      case _RD128     : return true;
      // stores
      case _WR8       : return true;
      case _WR16      : return true;
      case _WR32      : return true;
      case _WR64      : return true;
      case _WR128     : return true;
      // memory management
      case _ALLO      : return true;
      case _MLLO      : return true;
      case _FREE      : return true;
      case _CALL      : return true;
      case _RET      : return true;
      // threads
      case _THCREAT   : return true;
      // case _THSTART   : return ;
      case _THEXIT    : return true;
      case _THJOIN    : return true;
      case _THCTXSW   : return true;
      // locks
      case _MTXINIT   : return true;
      case _MTXLOCK   : return true;
      case _MTXUNLK   : return true;
      // misc
      case _NONE      : return true;
      }
   }
   return true;
}
