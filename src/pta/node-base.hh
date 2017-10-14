
#ifndef __STID_PTA_NODEBASE_HH_
#define __STID_PTA_NODEBASE_HH_

#include <set>

namespace stid {
namespace pta {

/// This represents a base class for storing pointers to other nodes. The type
/// parameter T represents the set of pointed objects.
template <typename T>
class NodeBase
{
protected:

   /// The set of memory nodes pointed by this memory node
   typedef std::set<T*> SuccSet;
   SuccSet succ;

   /// Returns true iff \p n is a memory location (possibly) pointed by this
   bool is_succ (T *n) const
   {
      return succ.find (n) != succ.end();
   }

public:

   // iterators over the set of pointed memory locations
   typedef typename SuccSet::iterator iterator;
   typedef typename SuccSet::const_iterator const_iterator;

   iterator begin () { return succ.begin(); }
   iterator end () { return succ.end(); }
   const_iterator begin () const { return succ.begin(); }
   const_iterator end () const { return succ.end(); }

   /// Number of memory locations pointed by this one
   size_t size() const { return succ.size(); }

   /// Returns true iff the set of successors is empty
   size_t empty() const { return succ.empty(); }

   /// Removes all successors
   void clear () { succ.clear (); }

   /// Adds one element to the set of successors.
   /// \returns True iff the added node was already there.
   bool add (T *n)
   {
      return !succ.insert(n).second;
   }

   /// Adds every successor of \p n to the set of successors of this node.
   /// \returns True iff every added node was already a successor of this node.
   bool include (T *n)
   {
      bool already = true;
      for (T *sn : *n) already = add (sn) && already;
      return already;
   }
};

} // pta
} // stid

#endif

