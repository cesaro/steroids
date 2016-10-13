
#ifndef __VCLOCK_HH_
#define __VCLOCK_HH_

#include "verbosity.h"

class vclockt
{
public:
   // zero
   inline vclockt (unsigned size) :
      size (size),
      tab (new int[size])
   {
      for (int i = 0; i < size; ++i) ASSERT (tab[i] == 0);
      for (int i = 0; i < size; ++i) tab[i] = 0;
   }
  
   // copy constructor 
   inline vclockt (const vclockt &v) :
      size (v.size),
      tab (new int[size])
   {
      for (int i = 0; i < size; ++i) tab[i] = v.tab[i];
   }
   
   // this = max(v1,v2)
   inline vclockt (const vclockt &v1, const vclockt &v2) :
      size (v1.size),
      tab (new int[size])
   {
      ASSERT (v1.size == v2.size);
      for (int i = 0; i < size; ++i) tab[i] = std::max (v1.tab[i], v2.tab[i]);
   }

   bool operator== (const vclockt &other) const;
   bool operator<= (const vclockt &other) const;
   bool operator>= (const vclockt &other) const;
   inline int &operator[] (unsigned idx)
      { ASSERT (idx < size); return tab[idx]; }
private:
   unsigned size; // we could move this field somewhere else global
   int * tab;
};

#endif

