
#include "action_stream.hh"

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
   uint8_t a = *trace.evptr;
   switch (a)
   {
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

   // call, ret, context switch: 1 argument, an id
   case RT_CALL:
   case RT_RET:
   case RT_THCREAT:
   case RT_THJOIN:
   case RT_THCTXSW:
      trace.idptr++;
      break;

   // exit: 0 arguments
   case RT_THEXIT:
      break;

   // mutex-{lock,unlock}: 1 argument, an address
   case RT_MTXLOCK:
   case RT_MTXUNLK:
      trace.addrptr++;
      break;

   // less common RD / WR events: 1 address and multiple words (2-31)
   case RT_RD128:
   case RT_WR128:
      trace.addrptr++;
      trace.valptr += 2;
      break;
   default:
      if (RT_IS_MULTIW_RD (a) || RT_IS_MULTIW_WR (a))
      {
         trace.addrptr++;
         trace.valptr += RT_MULTIW_COUNT (a);
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

bool action_stream_itt::has_addr ()
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
   default :
      ASSERT (0);
      return true;
   }
}

bool action_stream_itt::has_val ()
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
   default :
      ASSERT (0);
      return true;
   }
}

bool action_stream_itt::has_id ()
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
      case RT_THEXIT :
         return false;
      case RT_THCREAT :
      case RT_THJOIN :
      case RT_THCTXSW :
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

action_stream2t::action_stream2t (const action_streamt &s)
{
   unsigned i;
   actt a;

   for (auto &ac : s)
   {
      // actions don't have both address and id
      ASSERT (not ac.has_addr() or not ac.has_id());

      // store the action in a
      a.type = ac.type();
      a.addr = ac.has_addr() ? ac.addr() : (ac.has_id() ? ac.id() : 0);
      if (ac.has_val())
      {
         ASSERT (ac.val_size() < MAX_WORDS);
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

void action_stream2t::diff (const action_stream2t &other)
{
   size_t i, min;
   unsigned j;
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
               _rt_action_to_str (stream[i].type),
               _rt_action_to_str (other.stream[i].type),
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

