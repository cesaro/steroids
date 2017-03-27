
#include <bitset>
#include <vector>
#include <utility>
#include <stdlib.h>

#include "misc.hh"
#include "verbosity.h"
#include "checker.hh"
#include "vclock.hh"


namespace stid {

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

} // namespace
