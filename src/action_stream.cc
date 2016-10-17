
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

