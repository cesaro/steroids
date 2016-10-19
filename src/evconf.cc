
#include "evconf.hh"
#include "verbosity.h"

eventt::eventt (int num_ths, unsigned sidx) :
   _tid (0),
   _sidx (sidx),
   _pre_other (nullptr),
   act ({.type = action_typet::THSTART}),
   redbox (),
   vclock (num_ths)
{
   printf ("stid: po: eventt: ctor: this %p sidx %u num %d (bot)\n",
         this, sidx, num_ths);
}

eventt::eventt (unsigned sidx, eventt &creat, unsigned p) :
   _tid (p),
   _sidx (sidx),
   _pre_other (&creat),
   act ({.type = action_typet::THSTART}),
   redbox (),
   vclock (creat.vclock)
{
   printf ("stid: po: eventt: ctor: this %p sidx %u creat %p proc %u\n",
         this, sidx, &creat, p);
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
   printf ("stid: po: eventt: ctor: this %p sidx %u ac.type %s pre %p\n",
         this, sidx, actiont_type_str(ac.type), &p);
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
   printf ("stid: po: eventt: ctor: this %p sidx %u ac.type %s pre %p other %p\n",
         this, sidx, actiont_type_str(ac.type), &p, &m);
   vclock[_tid] += 2; // the +1 is the redbox events
}

unsigned eventt::idx (const conft &c)
{
   return (unsigned) (this - &c.events[_tid][0]);
}

void eventt::print (const conft &c)
{
   printf ("eventt this %18p tid %2d sidx %10d pos %4u ac.type %s |red| %10lu pre_proc %18p pre_mem %18p\n",
         this,
         _tid,
         _sidx,
         idx (c),
         actiont_type_str (act.type),
         redbox.size (),
         pre_proc (),
         pre_other ());
}

conft::conft (action_streamt &s) :
   mutexmax (),
   _stream (s)
{
   const struct rt *rt = s.get_rt ();

   num_ths = rt->trace.num_ths;
   ASSERT (num_ths > 0);
   for (int i=0; i < num_ths; i++)
   {
     events.push_back (std::vector<eventt> ());
     events[i].reserve (1000);
   }
   //num_mutex = rt->trace.num_mutex;

   //std::vector<eventt> mutexmax (num_mutex);
}

// For debug purposes only (FOR NOW)
void conft::print_original_stream ()
{
   _stream.print ();
}

// For debug purposes only (FOR NOW)
void conft::print ()
{
   // iterate throught the actions
   int tid = 0;
   printf ("Print partial order\n");
   for (auto &ths : events)
   {
      printf ("Thread %d, size %zu, first %p, last %p\n", 
            tid, ths.size(), &ths[0], &ths.back());
      for (auto &es : ths)
         es.print (*this);
      tid++;
   }
   printf ("Print mutex max\n");
   for (auto &e : mutexmax)
   {
     e.second->print (*this);
   }
}

void conft::build ()
{
   eventt *ev;
   actiont ac;

   // position in the stream
   int sidx = 0;

   // current thread identifier
   int cur_tid = 0;
 
   // safety check
   bool is_next_global = false;

   // store the create event per thread
   std::vector<eventt*> createvs(num_ths);

   // store the exit event per thread
   std::vector<eventt*> exitevs(num_ths);

   // if the previous event was a context
   // switch, we might just want to advance
   // on the action stream without adding events
   bool is_new_ev = true;

   // create the bottom event
   events[0].emplace_back (num_ths, sidx);
   ev = &events[0].back();
 
   auto st_it = _stream.begin ();
   while (st_it != _stream.end ())
   {
      if (is_new_ev)
      {
         is_next_global = add_red_events (st_it, sidx, *ev);
         // at this point, the red events are already in ev
         // is_next_global ensures that the next event of the
         // stream is a 'global' event
         ASSERT (is_next_global);
         // the iterator cannot be at the end of stream
         ASSERT (st_it != _stream.end ());
 
         // ev->act.pretty_print ();
      }

      is_new_ev = true;

      // the iterator now should be pointing to a new blue event
      auto act = *st_it++;
      switch (act.type ())
      { 
      // threads
      case RT_THCREAT :
         ac.type = action_typet::THCREAT;
         ac.val  = act.id ();
         events[cur_tid].emplace_back (sidx++, ac, events[cur_tid].back());
         ev = &events[cur_tid].back();
         createvs[ac.val] = ev;
         break;
      case RT_THEXIT :
         ac.type = action_typet::THEXIT;
         events[cur_tid].emplace_back (sidx++, ac, events[cur_tid].back());
         ev = &events[cur_tid].back();
         exitevs[cur_tid] = ev;
         break;
      case RT_THCTXSW :
         // change the current tid
         cur_tid = act.id ();
         // if there are no events in the cur_tid generate THSTART 
         if (events[cur_tid].size () == 0)
         {
            events[cur_tid].emplace_back (sidx++, *createvs[cur_tid], cur_tid);
            ev = &events[cur_tid].back();
         }
         else
         {
            is_new_ev = false;
            sidx++;
         }
         break;
      case RT_THJOIN :
         ac.type = action_typet::THJOIN;
         ac.val = act.id ();
         // assert that we have already seen the exit event
         ASSERT (exitevs[ac.val]->act.type == action_typet::THEXIT);
         events[cur_tid].emplace_back (sidx++, ac,
               events[cur_tid].back(), *exitevs[ac.val]);
         ev = &events[cur_tid].back();
         break; 
      case RT_MTXLOCK :
      {
         ac.type = action_typet::MTXLOCK;
         ac.addr = act.addr ();
         // create the event
         auto mut = mutexmax.find (ac.addr);
         if (mut == mutexmax.end ())
         {
             events[cur_tid].emplace_back (sidx++, ac, events[cur_tid].back());
             ev = &events[cur_tid].back();
             ASSERT (ev->pre_other () == nullptr);
         }
         else
         {
             events[cur_tid].emplace_back (sidx++, ac, events[cur_tid].back(),
                  *mut->second);
             ev = &events[cur_tid].back();
         }
         // update the value of mutexmax
         mutexmax[ac.addr] = ev;
         break;
      }
      case RT_MTXUNLK :
      {
         ac.type = action_typet::MTXUNLK;
         ac.addr = act.addr ();
         // create the event
         auto mut = mutexmax.find (ac.addr);
         ASSERT (mut != mutexmax.end ());
         events[cur_tid].emplace_back (sidx++, ac, events[cur_tid].back(),
               *mut->second);
         ev = &events[cur_tid].back();
         // update the value of mutexmax
         mutexmax[ac.addr] = ev;
         break;
      }
      default :
         ASSERT(false);
      }
   }
}

// @FIXME: Change the insertion of the actions
bool conft::add_red_events (action_stream_itt &it, int &i, eventt &b_ev)
{
   actiont ac;
   int ty;
   auto act = *it;

   while (act != _stream.end ()) 
   {
      ty = act.type ();
      switch (ty)
      {
      // loads
      case RT_RD8 : 
         b_ev.redbox.emplace_back (
            actiont {.type = action_typet::RD8,
             .addr = act.addr (),
             .val  = *act.val ()});
         break; 
      case RT_RD16 : 
         ac.type = action_typet::RD16;
         ac.addr = act.addr ();
         ac.val  = *act.val ();
         b_ev.redbox.push_back (ac);
         break; 
      case RT_RD32 : 
         ac.type = action_typet::RD32;
         ac.addr = act.addr ();
         ac.val  = *act.val ();
         b_ev.redbox.push_back (ac);
         break;
      case RT_RD64 : 
         ac.type = action_typet::RD64;
         ac.addr = act.addr ();
         ac.val  = *act.val ();
         b_ev.redbox.push_back (ac);
         break;
      // stores
      case RT_WR8 : 
         ac.type = action_typet::WR8;
         ac.addr = act.addr ();
         ac.val  = *act.val ();
         b_ev.redbox.push_back (ac);
         break;
      case RT_WR16 : 
         ac.type = action_typet::WR16;
         ac.addr = act.addr ();
         ac.val  = *act.val ();
         b_ev.redbox.push_back (ac);
         break;
      case RT_WR32 : 
         ac.type = action_typet::WR32;
         ac.addr = act.addr ();
         ac.val  = *act.val ();
         b_ev.redbox.push_back (ac);
         break;
      case RT_WR64 : 
         ac.type = action_typet::WR64;
         ac.addr = act.addr ();
         ac.val  = *act.val ();
         b_ev.redbox.push_back (ac);
         break;
      // memory management
      case RT_ALLOCA :
         ac.type = action_typet::MALLOC;
         ac.addr = act.addr ();
         ac.val  = *act.val ();
         b_ev.redbox.push_back (ac);
         break;
      case RT_MALLOC :
         ac.type = action_typet::MALLOC;
         ac.addr = act.addr ();
         ac.val  = *act.val ();
         b_ev.redbox.push_back (ac);
         break;
      case RT_FREE :
         ac.type = action_typet::FREE;
         ac.addr = act.addr ();
         b_ev.redbox.push_back (ac);
         break;
      case RT_CALL :
         // @TODO: Check that CALL -> MALLOC 
         ac.type = action_typet::MALLOC;
         ac.addr = act.id ();
         b_ev.redbox.push_back (ac);
         break;
      case RT_RET  :
         ac.type = action_typet::FREE;
         // store the id in the addr field for consistency of
         // the FREE action
         ac.addr = act.id ();
         b_ev.redbox.push_back (ac);
         break;
      // the remainder are global actions
      default : 
        if (RT_IS_MULTIW_RD (ty))
        {
           // min 5 max 31
           ac.type = action_typet::RD64;
           ac.addr = act.addr ();
           for (int i = 0; i < act.val_size (); i++)
           {
             ac.val = *act.val ();
             b_ev.redbox.push_back (ac);
           }
           break;
        }
        if (RT_IS_MULTIW_WR (ty))
        {
           // min 5 max 31
           ac.type = action_typet::WR64;
           ac.addr = act.addr ();
           for (int i = 0; i < act.val_size (); i++)
           {
             ac.val = *act.val ();
             b_ev.redbox.push_back (ac);
           }
           break;
        }

        return true;
      }
      act = *++it;
      i++;
   }

   // This line should be unreachable
   ASSERT (0);
   return false;
}
