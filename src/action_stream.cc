
#include "stid/action_stream.hh"
#include "verbosity.h"

namespace stid {

action_stream_itt::action_stream_itt (const action_streamt &s, bool begin)
{
   if (begin)
   {
      // restart the pointers
      trace.evptr = s.rt->trace.ev.begin;
      trace.addrptr = (uint64_t*) s.rt->trace.addr.begin;
      trace.valptr = (uint64_t*) s.rt->trace.val.begin;
      trace.idptr = (uint16_t*) s.rt->trace.id.begin;
   }
   else
   {
      // event pointer points to the end
      trace.evptr = s.rt->trace.ev.begin + s.rt->trace.size;
   }
}

action_stream_itt &action_stream_itt::action_stream_itt::operator++ ()
{
   uint8_t a = *trace.evptr;

#define ENABLE_LOAD_STORE_CALLS 1

   switch (a)
   {
#if ENABLE_LOAD_STORE_CALLS
   // loads, stores, malloc, alloca, mutex-init: 2 arguments, addr & value
   case RT_RD8:
   case RT_RD16:
   case RT_RD32:
   case RT_RD64:
   case RT_WR8:
   case RT_WR16:
   case RT_WR32:
   case RT_WR64:
   case RT_ALLOCA:
   case RT_MALLOC:
      trace.addrptr++;
      trace.valptr++;
      break;

   // free: 1 argument: an address
   case RT_FREE:
      trace.addrptr++;
      break;
   // call, ret, context switch, exitnz: 1 argument, an id
   case RT_CALL:
   case RT_RET:
#endif
   case RT_THCREAT:
   case RT_THJOIN:
   case RT_THCTXSW:
   case RT_EXITNZ:
      trace.idptr++;
      break;

   // abort: 0 arguments
   case RT_THEXIT:
   case RT_ABORT :
      break;

   // mutex-{lock,unlock}: 1 argument, an address
   case RT_MTXLOCK:
   case RT_MTXUNLK:
      trace.addrptr++;
      break;
#if ENABLE_LOAD_STORE_CALLS
   // less common RD / WR events: 1 address and multiple words (2-31)
   case RT_RD128:
   case RT_WR128:
      trace.addrptr++;
      trace.valptr += 2;
      break;
#endif
#undef ENABLE_LOAD_STORE_CALLS
   default:
      if (RT_IS_MULTIW_RDWR (a))
      {
         trace.addrptr++;
         trace.valptr += RT_MULTIW_COUNT (a);
         break;
      }
      ASSERT (0);
   }

   // in any case: the pointer to the stream of actions always advances
   trace.evptr++;
   return *this;
}

action_stream_itt action_stream_itt::action_stream_itt::operator++ (int)
{
   action_stream_itt ret = *this;
   this->operator++();
   return ret;
}

bool action_stream_itt::has_addr () const
{
   uint8_t t = type ();

   // reads, writes, alloca, malloc, free, lock, unlock
   switch (RT_ACTION_CLASS (t))
   {
   case RT_ACTION_CLASS_RD8:
   case RT_ACTION_CLASS_RD64:
   case RT_ACTION_CLASS_WR8:
   case RT_ACTION_CLASS_WR64:
      return true;
   case RT_ACTION_CLASS_MM:
      switch (t)
      {
      case RT_ALLOCA :
      case RT_MALLOC :
      case RT_FREE :
         return true;
      case RT_CALL :
      case RT_RET :
         return false;
      default :
         ASSERT (0);
         return true;
      }
   case RT_ACTION_CLASS_TH:
      switch (t)
      {
      case RT_MTXLOCK :
      case RT_MTXUNLK :
         return true;
      case RT_THCREAT :
      case RT_THJOIN :
      case RT_THEXIT :
      case RT_THCTXSW :
         return false;
      default :
         ASSERT (0);
         return true;
      }
   case RT_ACTION_CLASS_WARN :
      switch (t)
      {
      case RT_ABORT :
      case RT_EXITNZ :
         return false;
      default :
         ASSERT (0);
         return true;
      }
   default :
      ASSERT (0);
      return true;
   }
}

bool action_stream_itt::has_val () const
{
   uint8_t t = type ();

   // reads, writes, alloca, malloc
   switch (RT_ACTION_CLASS (t))
   {
   case RT_ACTION_CLASS_RD8:
   case RT_ACTION_CLASS_RD64:
   case RT_ACTION_CLASS_WR8:
   case RT_ACTION_CLASS_WR64:
      return true;
   case RT_ACTION_CLASS_MM:
      switch (t)
      {
      case RT_ALLOCA :
      case RT_MALLOC :
         return true;
      case RT_FREE :
      case RT_CALL :
      case RT_RET :
         return false;
      default :
         ASSERT (0);
         return true;
      }
   case RT_ACTION_CLASS_TH:
      return false;
   case RT_ACTION_CLASS_WARN :
      switch (t)
      {
      case RT_ABORT :
      case RT_EXITNZ :
         return false;
      default :
         ASSERT (0);
         return true;
      }
   default :
      ASSERT (0);
      return true;
   }
}

bool action_stream_itt::has_id () const
{
   uint8_t t = type ();

   // reads, writes, alloca, malloc, call, ret, creat, join, cs
   switch (RT_ACTION_CLASS (t))
   {
   case RT_ACTION_CLASS_RD8:
   case RT_ACTION_CLASS_RD64:
   case RT_ACTION_CLASS_WR8:
   case RT_ACTION_CLASS_WR64:
      return false;
   case RT_ACTION_CLASS_MM:
      switch (t)
      {
      case RT_CALL :
      case RT_RET :
         return true;
      case RT_ALLOCA :
      case RT_MALLOC :
      case RT_FREE :
         return false;
      default :
         ASSERT (0);
         return true;
      }
   case RT_ACTION_CLASS_TH:
      switch (t)
      {
      case RT_MTXLOCK :
      case RT_MTXUNLK :
         return false;
      case RT_THEXIT :
      case RT_THCREAT :
      case RT_THJOIN :
      case RT_THCTXSW :
         return true;
      default :
         ASSERT (0);
         return true;
      }
   case RT_ACTION_CLASS_WARN :
      switch (t)
      {
      case RT_ABORT :
         return false;
      case RT_EXITNZ :
         return true;
      default :
         ASSERT (0);
         return true;
      }
   default :
      ASSERT (0);
      return true;
   }
}

   // MALLOC   0x1122334411223344, 0x100
   // ALLOCA   0x1122334411223344, 0x10
   // FREE     0x182391293
   // WR64     *0x1122334411223344 =  0x1122334411223344
   // RD64     *0x1122334411223344 == 0x1122334411223344
   // RD[64x8] *0x1122334411223344 == 0x1122334411223344 ...
   // WR[64x8] *0x1122334411223344 =  0x1122334411223344 ...
   // THCREAT  123
   // THSTART  123
   // THJOIN   123
   // THEXIT   123
   // MTX-INIT 0x1122334411223344, 0x1133
   // MTX-LOCK 0x1122334411223344
   // MTX-UNLK 0x1122334411223344


const char *action_stream_itt::str () const
{
   static char str[128];
   const char *action = __rt_action_to_str (type ());
   const char *eq = "";
   int a = type ();

   switch (a)
   {
   // most common loads
   case RT_RD8     :
   case RT_RD16    :
   case RT_RD32    :
   case RT_RD64    :
   case RT_RD128   :
   case RT_RD192   :
   case RT_RD256   :
      eq = "=";

   // most common stores
   case RT_WR8     :
   case RT_WR16    :
   case RT_WR32    :
   case RT_WR64    :
   case RT_WR128   :
   case RT_WR192   :
   case RT_WR256   :
      sprintf (str, "%s *%#-18lx =%s %#-18lx", action, addr(), eq, *val());
      break;

   // memory management
   case RT_ALLOCA  :
   case RT_MALLOC  :
      sprintf (str, "%s %#-18lx, %#-18lx", action, addr(), *val());
      break;

   // call, ret, threads, exit
   case RT_CALL    :
   case RT_RET     :
   case RT_THCREAT :
   case RT_THJOIN  :
   case RT_THCTXSW :
   case RT_EXITNZ :
      sprintf (str, "%s %#x", action, id());
      break;

   // exit, abort
   case RT_THEXIT  :
   case RT_ABORT :
      sprintf (str, "%s", action);
      break;

   // mutex locks, free
   case RT_FREE    :
   case RT_MTXLOCK :
   case RT_MTXUNLK :
      sprintf (str, "%s %#-18lx", action, addr());
      break;

   // less common loads / stores
   default :
      if (RT_IS_MULTIW_RD (a))
      {
         action = "RD";
         eq = "=";
      }
      else if (RT_IS_MULTIW_WR (a))
      {
         action = "WR";
      }
      else
      {
         ASSERT (0);
      }
      sprintf (str, "%s[64x%d] *%#-18lx =%s %#-18lx, ...",
            action, RT_MULTIW_COUNT(a), addr(), eq, *val());
      break;
   }

   return str;
}


void action_streamt::print (int limit) const
{
   // iterate throught the actions
   int i = 0;
   int tid = 0;

   printf ("== action stream begin ==\n");
   printf ("size %zu, num_ths %d, replay len %d\n",
      rt->trace.size, rt->trace.num_ths, rt->replay.size);

   printf ("expected number of blue events per thread:\n");
   ASSERT (rt->trace.num_ths <= RT_MAX_THREADS);
   for (i = 0; i < rt->trace.num_ths; i++)
   {
      printf ("      %2d: %zu blues\n", i, rt->trace.num_blue[i]);
   }
   for (; i < RT_MAX_THREADS; i++) ASSERT (rt->trace.num_blue[i] == 0);

   i = 0;
   printf ("\nstream:\n");
   for (auto &ac : *this)
   {
      if (ac.type() == RT_THCTXSW) tid = ac.id();
      printf ("%05u %2d: %s\n", i, tid, ac.str());
      i++;
      if (limit > 0 and i > limit)
      {
         printf ("... (skipping remaining actions until the end)\n");
         break;
      }
#if 0
      // for efficiency purposes ac has type "action_stream_itt" rather than
      // "actiont"
      printf ("idx %5d action %#4x '%s' addr %#18lx val[0] %#18lx valsize %u "
            "id %#10x\n",
            i,
            ac.type (),
            _rt_action_to_str (ac.type ()),
            ac.addr (),
            *ac.val (),
            ac.val_size(),
            ac.id ());
#endif
   }
   printf ("== action stream end ==\n");
}

Replay action_streamt::get_replay ()
{
   unsigned i;
   Replay replay;
   struct replayevent ctx;
   int sawfirst[RT_MAX_THREADS];
   int blue[RT_MAX_THREADS];
   int lastexit = 0;

   (void) lastexit; // for ASSERT

   // clean the sawfirst array
   ASSERT (rt->trace.num_ths <= RT_MAX_THREADS);
   for (i = 0; i < RT_MAX_THREADS; i++) sawfirst[i] = 0;

   // first action is always from the main thread
   sawfirst[0] = 1;
   ctx.tid = 0;
   ctx.count = 1;

   for (auto &ac : *this)
   {
      // if previous one was exit, then this one needs to be context switch or
      // exit non-zero
      ASSERT (not lastexit or ac.type() == RT_THCTXSW or ac.type() == RT_EXITNZ);


      switch (ac.type())
      {
      case RT_THCTXSW :
         if (ac.id() == ctx.tid) continue;
         lastexit = 0;
         replay.push_back (ctx);
         ctx.tid = ac.id();
         ASSERT (ctx.tid < rt->trace.num_ths);
         if (sawfirst[ctx.tid])
         {
            ctx.count = 0;
         }
         else
         {
            sawfirst[ctx.tid] = 1;
            ctx.count = 1;
         }
         break;

      case RT_THCREAT :
      case RT_THJOIN  :
      case RT_MTXLOCK :
      case RT_MTXUNLK :
         lastexit = 0;
         ctx.count++;
         break;

      case RT_THEXIT  :
         lastexit = 1;
         ctx.count++;
         break;

      default :
         // make sure here that we didn't miss anything!
         break;
      }
   }
   replay.push_back (ctx); // last context switch
   replay.push_back ({-1, -1}); // end of replay, free mode!

   // in this replay, the number of blue events per thread should equal that
   // reported by the runtime; assert it
   for (i = 0; i < RT_MAX_THREADS; i++) blue[i] = 0;
   for (i = 0; i < replay.size(); i++)
   {
      if (replay[i].tid == -1) continue;
      blue[replay[i].tid] += replay[i].count;
   }
   for (i = 0; i < RT_MAX_THREADS; i++)
      ASSERT (blue[i] == rt->trace.num_blue[i]);
   return replay;
}

void action_streamt::print_replay ()
{
   print_replay (get_replay ());
}

void action_streamt::print_replay (std::vector<struct replayevent> replay)
{
   unsigned i;
   printf ("== replay begin ==\n");
   for (i = 0; i < replay.size(); i++)
   {
      if (replay[i].tid == -1)
         printf ("%05u %2d: end of replay\n", i, -1);
      else
         printf ("%05u %2d: %d\n", i, replay[i].tid, replay[i].count);
   }
   printf ("== replay end ==\n");
}

action_stream2t::action_stream2t (const action_streamt &s)
{
   unsigned i;
   actt a;

   // reserve space in the vector
   stream.reserve (s.size ());

   for (auto &ac : s)
   {
      // actions don't have both address and id
      ASSERT (not ac.has_addr() or not ac.has_id());

      // store the action in a
      a.type = ac.type();
      a.addr = ac.has_addr() ? ac.addr() : (ac.has_id() ? ac.id() : 0);
      if (ac.has_val())
      {
         ASSERT (ac.val_size() <= MAX_WORDS);
         for (i = 0; i < ac.val_size(); i++) a.val[i] = ac.val()[i];
         for (; i < MAX_WORDS; i++) a.val[i] = 0;
      }
      else
      {
         for (i = 0; i < MAX_WORDS; i++) a.val[i] = 0;
      }

      // make a copy in the vector
      stream.push_back (a);
   }
}

void action_stream2t::diff (const action_stream2t &other, optt opt)
{
   size_t i, min;
   unsigned j, differences;
   bool spotted;

   printf (
R"XX(== diff begin ==
Index / What      Stream 1                Stream 2                Difference spotted
================= ======================= ======================= ==================
this              %-23p %-23p %s
size              %-23zu %-23zu %s
)XX",
         this, &other,
         this != &other ? "!!" : "",
         this->stream.size(), other.stream.size(),
         this->stream.size() != other.stream.size() ? "!!" : ""
         );

   min = std::min (stream.size(), other.stream.size());
   differences = 0;
   for (i = 0; i < min; i++)
   {
      spotted = false;
      if (stream[i].type != other.stream[i].type) spotted = true;
      if (stream[i].addr != other.stream[i].addr) spotted = true;
      for (j = 0; j < MAX_WORDS; j++)
         if (stream[i].val[j] != other.stream[i].val[j]) spotted = true;
      if (spotted)
      {
         printf (
R"XX(-
idx %-13zu type %-18s type %-18s %s
                  addr %-#18lx addr %-#18lx %s
)XX",
               i,
               __rt_action_to_str (stream[i].type),
               __rt_action_to_str (other.stream[i].type),
               stream[i].type != other.stream[i].type ? "!!" : "",
               stream[i].addr, other.stream[i].addr,
               stream[i].addr != other.stream[i].addr ? "!!" : ""
               );

         for (j = 0; j < MAX_WORDS; j++)
         {
            printf ("                  v%02u  %-#18lx v%02u  %-#18lx %s\n",
                  j, stream[i].val[j],
                  j, other.stream[i].val[j],
                  stream[i].val[j] != other.stream[i].val[j] ? "!!" : "");
         }
         differences++;
      }
      if (differences and opt == SPOT_FIRST)
      {
         printf ("-\nidx %zu to %zu not examined\n", i+1, min);
         break;
      }
   }

   if (this->stream.size() != other.stream.size())
   {
      char str1[48];
      char str2[48];
      sprintf (str1, "%zu more actions", this->stream.size() - min);
      sprintf (str2, "%zu more actions", other.stream.size() - min);

      printf ("-\nidx %-13zu %-23s %-23s !!\n", min, str1, str2);
   }

   printf ("== diff end ==\n");
}

} // namespace
