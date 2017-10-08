#ifndef __STID_PTA_WORKLIST_HH_
#define __STID_PTA_WORKLIST_HH_

#include <deque>
#include <set>

#include "verbosity.h"

namespace stid {
namespace pta {

/// A FIFO list that prevents the insertion of duplicate elements
template<typename T>
class Worklist
{
private :
   typedef std::deque<T> Deque;
   typedef std::set<T> Set;

   Deque deque;
   Set set;

public :
   typedef typename Deque::iterator iterator;
   typedef typename Deque::const_iterator const_iterator;

   void clear ()
   {
      deque.clear ();
      set.clear ();
   }

   bool push (T e)
   {
      ASSERT (deque.size() == set.size());
      if (contained (e)) return false;
      deque.push_back (e);
      set.insert (e);
      return true;
   }

   T pop ()
   {
      ASSERT (deque.size() >= 1);
      T e = deque.front();
      deque.pop_front();
      set.erase (e);
      ASSERT (deque.size() == set.size());
      return e;
   }

   T front () const
   {
      ASSERT (deque.size() >= 1);
      return deque.front ();
   }

   T back () const
   {
      ASSERT (deque.size() >= 1);
      return deque.back ();
   }

   size_t size () const
   {
      ASSERT (deque.size() == set.size());
      return deque.size();
   }

   bool empty () const
   {
      ASSERT (deque.size() == set.size());
      return deque.empty();
   }

   bool contained (T e) const
   {
      return set.find(e) != set.end();
   }

   iterator begin() { return deque.begin(); }
   iterator end() { return deque.end(); }

   const_iterator begin() const { return deque.begin(); }
   const_iterator end() const { return deque.end(); }
};

} // pta
} // stid

#endif
