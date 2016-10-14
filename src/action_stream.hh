
#ifndef __ACTION_STREAM_HH_
#define __ACTION_STREAM_HH_

#include "action.hh"
#include "../rt/rt.h"

class action_streamt;

class action_stream_itt
{
public:
   inline bool operator== (const action_stream_itt &other) const
      { return trace.evptr == other.trace.evptr; }
   inline bool operator!= (const action_stream_itt &other) const
      { return trace.evptr != other.trace.evptr; }
   action_stream_itt &operator++ ();
   action_stream_itt operator++ (int);

   inline action_stream_itt &operator* ()
      { return *this; }

   inline int type ()
      { return *trace.evptr; }
   inline uint64_t addr ()
      { return *trace.addrptr; }
   inline uint64_t val ()
      { return *trace.valptr; }
   inline uint64_t val2 ()
      { return trace.valptr[1]; }
   inline uint16_t id ()
      { return *trace.idptr; }
   
private:
   action_stream_itt (const action_streamt &s, bool begin);
   struct eventrace trace; // copy of that in the stream
   friend class action_streamt;
};

class action_streamt
{
public:
   action_streamt (struct rt *rt) :
      rt (rt) {}
   action_stream_itt begin () const
      { return action_stream_itt (*this, true); }
   action_stream_itt end () const
      { return action_stream_itt (*this, false); }

   // @TODO: rt should be const
   inline struct rt * get_rt()
      { return rt; }

private:
   struct rt *rt;
   friend class action_stream_itt;
};


#endif
