
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

