
#include <bitset>
#include <vector>
#include <utility>
#include <stdlib.h>

#include "misc.hh"
#include "verbosity.h"
#include "checker.hh"

const char *actiont::type_str ()
{
   switch (type)
   {
   // loads
   case action_typet::RD8       : return "RD8     ";
   case action_typet::RD16      : return "RD16    ";
   case action_typet::RD32      : return "RD32    ";
   case action_typet::RD64      : return "RD64    ";
   // stores
   case action_typet::WR8       : return "WR8     ";
   case action_typet::WR16      : return "WR16    ";
   case action_typet::WR32      : return "WR32    ";
   case action_typet::WR64      : return "WR64    ";
   // memory management
   case action_typet::MALLOC    : return "MALLOC  ";
   case action_typet::FREE      : return "FREE    ";
   // threads
   case action_typet::THCREAT   : return "THCREAT ";
   case action_typet::THSTART   : return "THCREAT ";
   case action_typet::THEXIT    : return "THEXIT  ";
   case action_typet::THJOIN    : return "THJOIN  ";
   // locks
   case action_typet::MTXINIT   : return "MTX-INIT";
   case action_typet::MTXLOCK   : return "MTX-LOCK";
   case action_typet::MTXUNLK   : return "MTX-UNLK";
   }
}

void actiont::pretty_print ()
{
   // MALLOC   0x1122334411223344, 0x1122334411223344B
   // FREE     0x182391293
   // WR64     *0x1122334411223344 =  0x1122334411223344
   // RD64     *0x1122334411223344 == 0x1122334411223344
   // THCREAT  123
   // THSTART  123
   // THJOIN   123
   // THEXIT   123
   // MTX-INIT 0x1122334411223344, 0x1133
   // MTX-LOCK 0x1122334411223344
   // MTX-UNLK 0x1122334411223344

   const char *eq = "";
   switch (type)
   {
   // loads
   case action_typet::RD8       :
   case action_typet::RD16      :
   case action_typet::RD32      :
   case action_typet::RD64      :
      eq = "=";

   // stores
   case action_typet::WR8       :
   case action_typet::WR16      :
   case action_typet::WR32      :
   case action_typet::WR64      :
      printf ("%s *%#-18lx =%s %#-18lx\n",
            type_str (), addr, eq, val);
      break;

   case action_typet::MALLOC    :
   case action_typet::MTXINIT   :
      printf ("%s %#-18lx, %#-18lx\n", type_str (), addr, val);
      break;

   case action_typet::FREE      :
   case action_typet::MTXLOCK   :
   case action_typet::MTXUNLK   :
      printf ("%s %#-18lx\n", type_str (), addr);
      break;

   case action_typet::THCREAT   :
   case action_typet::THSTART   :
   case action_typet::THEXIT    :
   case action_typet::THJOIN    :
      printf ("%s %u\n", type_str (), (unsigned) val);
      break;
   }
}

action_stream_itt::action_stream_itt (const action_streamt &s, bool begin) :
   trace (s.rt->trace) // we make a copy, this saves 1 memory access in future operations
{
   if (begin)
   {
      // restart the pointers
      trace.evptr = trace.ev.begin;
      trace.addrptr = (uint64_t*) trace.addr.begin;
      trace.valptr = (uint64_t*) trace.val.begin;
      trace.idptr = (uint16_t*) trace.id.begin;
   }
   else
   {
      // event pointer points to the end
      trace.evptr = trace.ev.begin + trace.size;
   }
}

action_stream_itt &action_stream_itt::action_stream_itt::operator++ ()
{
   switch (*trace.evptr)
   {
   // loads, stores, malloc, alloca, mutex-init: 2 arguments, addr & value
   case _RD8:
   case _RD16:
   case _RD32:
   case _RD64:
   case _WR8:
   case _WR16:
   case _WR32:
   case _WR64:
   case _ALLO:
   case _MLLO:
   case _MTXINIT:
      trace.addrptr++;
      trace.valptr++;
      break;

   // 128 bit loads and stores have two 64bit words in val, and 1 address
   case _RD128:
   case _WR128:
      trace.addrptr++;
      trace.valptr += 2;
      break;

   // free: 1 argument: an address
   case _FREE:
      trace.addrptr++;
      break;

   // call, ret, context switch: 1 argument, an id
   case _CALL:
   case _RET:
   case _THCREAT:
   case _THJOIN:
   case _THSW:
      trace.idptr++;
      break;

   // exit: 0 arguments
   case _THEXIT:
   case _NONE:
      break;

   // mutex-{lock,unlock}: 1 argument, an address
   case _MTXLOCK:
   case _MTXUNLK:
      trace.addrptr++;
      break;
   }

   // in any case: the pointer to the stream of actions always advances
   trace.evptr++;
   return *this;
}


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

inline vclockt::vclockt (unsigned size) :
   size (size),
   tab (new int[size])
{
   for (int i = 0; i < size; ++i) ASSERT (tab[i] == 0);
   for (int i = 0; i < size; ++i) tab[i] = 0;
}

inline vclockt::vclockt (const vclockt &v) :
   size (v.size),
   tab (new int[size])
{
   for (int i = 0; i < size; ++i) tab[i] = v.tab[i];
}

inline vclockt::vclockt (const vclockt &v1, const vclockt &v2) :
   size (v1.size),
   tab (new int[size])
{
   ASSERT (v1.size == v2.size);
   for (int i = 0; i < size; ++i) tab[i] = std::max (v1.tab[i], v2.tab[i]);
}



#if 0
void Checker::run ()
{
   /*
   * Check for parity of lock/unlock (same thread) and no double lock/unlock
   *
   *  Datastructures: 
   *     unordered_map<addr, int> locks_idx              // use hash :: addr -> int
   *     bitset<num_lock> locks_val                      // initially 0 
   *     array<int, num_lock> locks_own                  // initially 0 
   *
   *  Semantics & Checks:
   *     LOCK addr
   *       int addr_idx = hash (addr);
   *       locks_val[addr_idx] ^= 1;                     // xor 1 with the current value
   *       error_double_lock (locks_val[addr_idx] == 0); // if the new value is 0, we have a double lock
   *       locks_own[addr_idx] = tid;                    // set this lock to be owned by thread tid 
   *     
   *     UNLOCK addr
   *       int addr_idx = hash (addr);
   *       locks_val[addr_idx] ^= 0;                     // xor 0 with the current value
   *       error_double_lock (locks_val[addr_idx] == 0); // if the new value is 0, we have a double unlock
   *       error_unlock (locks_own[addr_idx] != tid);    // if the thread does not own the lock, UB 
   *       locks_own[addr_idx] = -1;                     // reset the ownership of the lock 
   *
   *     END
   *       error_parity (locks_val != 0);                // at the end, the locks_val should be 0 
   *       
   */

   /*
   * Check for deadlocks and actions after EXIT/JOIN tid
   *
   *  Datastructures: 
   *     bitset<num_ths> exits                           // initially all 1s: 2^(num_ths+1) - 1 
   *     bitset<num_ths> joins                           // initially 0 
   *
   *  Semantics & Checks:
   *     EXIT
   *       exits[tid] = 0;                               // set tid exit to 0 
   *    
   *     JOIN o_tid
   *       error_join_before_exit (exits[o_tid] == 1);   // we have a join o_tid before the o_tid exited
   *       joins[o_tid] ^= 1;
   * 
   *     ANY ACTION 
   *       error_act_after_exit (exits[tid] == 0);       // we have already exited this thread 
   *
   *     END
   *       error_parity (exits != 0);                    // at the end, the exits should be 0 
   *       
   */

   /*
   * Check that CREATE increments the thread counter 
   *
   *  Datastructures: 
   *     int tid_cntr                                    // initially 1 
   *
   *  Semantics & Checks:
   *     CREATE tid 
   *       error_parity (tid != tid_cntr++);             // the next create is the current counter 
   *       
   */
 
   DEBUG ("checker: starting checker");
}

#endif
