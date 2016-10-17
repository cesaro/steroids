
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
   inline uint64_t *val ()
      { return trace.valptr; }
   inline unsigned val_size ()
      { return RT_IS_MULTIW_RDWR(type()) ? RT_MULTIW_COUNT(type()) : 1; }
   inline uint16_t id ()
      { return *trace.idptr; }

   inline bool has_addr ();
   inline bool has_val ();
   inline bool has_id ();
   
private:
   action_stream_itt (const action_streamt &s, bool begin);
   struct eventrace trace; // copy of that in the stream
   friend class action_streamt;
};

class action_streamt
{
public:
   action_streamt (const struct rt *rt) :
      rt (rt) {}
   action_stream_itt begin () const
      { return action_stream_itt (*this, true); }
   action_stream_itt end () const
      { return action_stream_itt (*this, false); }

   // @TODO : should we have this here, or only get_trace() ??
   inline const struct rt * get_rt()
      { return rt; }
   inline size_t size () const
      { return rt->trace.size; }
   void print () const;

private:
   const struct rt *rt;
   friend class action_stream_itt;
};

class action_stream2t
{
public:
   static const unsigned MAX_WORDS = 4;
   typedef struct
   {
      int type;
      uint64_t addr;
      uint64_t val[MAX_WORDS];

      inline unsigned val_size ()
         { return RT_MULTIW_COUNT (type); }
   } actt;
   typedef enum
   {
      FULL,
      SPOT_FIRST
   } optt;

   std::vector<actt> stream;

   action_stream2t (const action_streamt &s);
   void diff (const action_stream2t &other, optt opt = FULL);
};

#endif
