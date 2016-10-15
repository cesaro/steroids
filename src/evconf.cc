
#include "evconf.hh"
#include "verbosity.h"

eventt::eventt () :
   _tid (0),
   _sidx (0),
   _pre_other (nullptr),
   act ({.type = action_typet::THSTART}),
   redbox (),
   vclock (1)
{}

eventt::eventt (int num_ths, unsigned sidx) :
   _tid (0),
   _sidx (sidx),
   _pre_other (nullptr),
   act ({.type = action_typet::THSTART}),
   redbox (),
   vclock (num_ths)
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

eventt::eventt (unsigned sidx, actiont ac, eventt &p) :
   _tid (p._tid),
   _sidx (sidx),
   _pre_other (nullptr),
   act (ac), 
   redbox (),
   vclock (p.vclock)
{
   vclock[_tid] += 2; // the +1 is the redbox events
}

eventt::eventt (unsigned sidx, actiont ac, eventt &p, eventt &m) :
   _tid (p._tid),
   _sidx (sidx),
   _pre_other (&m),
   act (ac), 
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
   ASSERT (num_ths > 0);
   for (int i=0; i < num_ths; i++)
     events.push_back (std::vector<eventt> ());

   num_mutex = rt->trace.num_mutex;

   // std::vector<eventt> mutexmax (num_mutex);
}

// For debug purposes only (FOR NOW)
void conft::print_original_stream ()
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

// For debug purposes only (FOR NOW)
void conft::print ()
{
   // iterate throught the actions
   int tid = 0;
   int i = 0;
   for (auto ths : events)
   {
      printf ("Printing events of thread %2d\n", tid++);
      for (auto es : ths)
      {
      // for efficiency purposes ac has type "action_stream_itt" rather than "actiont"
      printf ("idx %5d type %2d\n",
         i,
         es.act.type);
      i++;
      }
   }
}

void conft::build ()
{
   // position in the stream
   int sidx = 0;

   // current thread identifier
   int cur_tid = 0;
 
   // type of the actions
   int type;
 
   // safety check
   bool is_next_global = false;
 
   // store the create event per thread
   std::vector<eventt> createvs(num_ths);

   // store the exit event per thread
   std::vector<eventt> exitevs(num_ths);

   // create the bottom event
   eventt ev = eventt (num_ths, sidx);
 
   auto st_it = _stream.begin ();
   actiont ac;
   while (st_it != _stream.end ())
   {
      is_next_global = add_red_events (st_it, sidx, ev);
      // at this point, the red events are already in ev
      // is_next_global ensures that the next event of the
      // stream is a 'global' event
      ASSERT (is_next_global);
      // the iterator cannot be at the end of stream
      ASSERT (st_it != _stream.end ());
 
      // add the blue event to its thread
      events[cur_tid].push_back (ev);
 
      // the iterator now should be pointing to a new blue event
      auto act = *st_it++;
      type = act.type ();
      switch (type)
      { 
      // threads
      case _THCREAT :
         ac.type = action_typet::THCREAT;
         ac.val  = act.id ();
         // verify this code
         ev = eventt (sidx++, ac, ev);
         createvs[ac.val] = ev;
         break;
      case _THEXIT : 
         ac.type = action_typet::THEXIT;
         ev = eventt (sidx++, ac, ev);
         exitevs[cur_tid] = ev;
         events[cur_tid].push_back (ev);
         break;
         // act = *st_it++;
      case _THCTXSW :
         // change the current tid
         cur_tid = act.id ();
         // if there are not events in the cur_tid generate THSTART 
         if (events[cur_tid].size () == 0)
         {
           ev = eventt (sidx++, createvs[cur_tid], cur_tid);
           break; 
         }
         else
         {
            // @TODO: check invariant.
            // if it is not the case that the we are in the beginning of a
            // new thread, we move to the next event which can only be a 
            // JOIN or a LOCK. Thus, we simply increment the iterator and 
            // do not break. Its crucial that JOIN and LOCK occur after
            // this case. 
            act = *st_it++; 
         }
      case _THJOIN :
        ac.type = action_typet::THJOIN;
        ac.val = act.id ();
        // assert that we have already seen the exit event
        ASSERT (exitevs[ac.val].act.type == action_typet::THEXIT);
        ev = eventt (sidx++, ac, ev, exitevs[ac.val]);
        break; 
      case _MTXINIT :
      {
        ac.type = action_typet::MTXINIT;
        ac.addr = act.addr ();
        // create the event
        ev = eventt (sidx++, ac, ev);
        // assert that the mutexmax for this addr is empty
        auto mut = mutexmax.find (ac.addr);
        // @TODO: check this
        ASSERT (mut == mutexmax.end ());
        // update the value of mutexmax
        mutexmax[ac.addr] = &ev;
        break; 
      }
      case _MTXLOCK :
      {
        ac.type = action_typet::MTXLOCK;
        ac.addr = act.addr ();
        // create the event
        auto mut = mutexmax.find (ac.addr);
        // should we enforce that the init must happen?
        // ASSERT (mut != mut.end ()); 
        if (mut == mutexmax.end ())
        {
            ev = eventt (sidx++, ac, ev);
        }
        else
        {
            ev = eventt (sidx++, ac, ev, *mut->second);
        }
        // update the value of mutexmax
        mutexmax[ac.addr] = &ev;
        break;
      } 
      case _MTXUNLK : 
      {
        ac.type = action_typet::MTXUNLK;
        ac.addr = act.addr ();
        // create the event
        auto mut = mutexmax.find (ac.addr);
        ASSERT (mut != mutexmax.end ()); 
        ev = eventt (sidx++, ac, ev, *mut->second);
        // update the value of mutexmax
        mutexmax[ac.addr] = &ev;
        break;
      } 
      }
   }
}

bool conft::add_red_events (action_stream_itt &it, int &i, eventt &b_ev)
{
   int type;
   actiont ac;
   auto act = *it;
   while (act != _stream.end ()) 
   {
      type = act.type ();
      printf ("red event of type %d\n", type); 
      switch (type)
      {
      // loads
      case _RD8 : 
         ac.type = action_typet::RD8;
         ac.addr = act.addr ();
         ac.val  = act.val ();
         b_ev.redbox.push_back (ac);
         break; 
      case _RD16 : 
         ac.type = action_typet::RD16;
         ac.addr = act.addr ();
         ac.val  = act.val ();
         b_ev.redbox.push_back (ac);
         break; 
      case _RD32 : 
         ac.type = action_typet::RD32;
         ac.addr = act.addr ();
         ac.val  = act.val ();
         b_ev.redbox.push_back (ac);
         break;
      case _RD64 : 
         ac.type = action_typet::RD64;
         ac.addr = act.addr ();
         ac.val  = act.val ();
         b_ev.redbox.push_back (ac);
         break;
      case _RD128 : 
         ac.type = action_typet::RD64;
         ac.addr = act.addr ();
         ac.val  = act.val ();
         b_ev.redbox.push_back (ac);
         ac.val  = act.val2 ();
         b_ev.redbox.push_back (ac);
         break;
      // stores
      case _WR8 : 
         ac.type = action_typet::WR8;
         ac.addr = act.addr ();
         ac.val  = act.val ();
         b_ev.redbox.push_back (ac);
         break;
      case _WR16 : 
         ac.type = action_typet::WR16;
         ac.addr = act.addr ();
         ac.val  = act.val ();
         b_ev.redbox.push_back (ac);
         break;
      case _WR32 : 
         ac.type = action_typet::WR32;
         ac.addr = act.addr ();
         ac.val  = act.val ();
         b_ev.redbox.push_back (ac);
         break;
      case _WR64 : 
         ac.type = action_typet::WR64;
         ac.addr = act.addr ();
         ac.val  = act.val ();
         b_ev.redbox.push_back (ac);
         break;
      case _WR128 : 
         ac.type = action_typet::WR64;
         ac.addr = act.addr ();
         ac.val  = act.val ();
         b_ev.redbox.push_back (ac);
         ac.val  = act.val2 ();
         b_ev.redbox.push_back (ac);
         break;
      // memory management
      case _ALLO :
         ac.type = action_typet::MALLOC;
         ac.addr = act.addr ();
         ac.val  = act.val ();
         b_ev.redbox.push_back (ac);
         break;
      case _MLLO :
         ac.type = action_typet::MALLOC;
         ac.addr = act.addr ();
         ac.val  = act.val ();
         b_ev.redbox.push_back (ac);
         break;
      case _FREE :
         ac.type = action_typet::FREE;
         ac.addr = act.addr ();
         b_ev.redbox.push_back (ac);
         break;
      case _CALL :
         // @TODO: Check that CALL -> MALLOC 
         ac.type = action_typet::MALLOC;
         ac.addr = act.id ();
         b_ev.redbox.push_back (ac);
         break;
      case _RET  :
         ac.type = action_typet::FREE;
         // store the id in the addr field for consistency of
         // the FREE action
         ac.addr = act.id ();
         b_ev.redbox.push_back (ac);
         break;
      // misc
      case _NONE : return false;
      // the remainder are global actions
      default : 
        printf ("going to exit the red event box\n");
        return true;
      }
      act = *++it;
   } 

   // This code should be unreachable
   ASSERT (0);
   return false;
}
