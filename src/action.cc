
#include "action.hh"

const char *actiont_type_str (unsigned t)
{
   return actiont_type_str ((action_typet) t);
}

const char *actiont_type_str (action_typet t)
{
   switch (t)
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
   case action_typet::THSTART   : return "THSTART ";
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
   // THEXIT   
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
            actiont_type_str (type), addr, eq, val);
      break;

   case action_typet::MALLOC    :
   case action_typet::MTXINIT   :
      printf ("%s %#-18lx, %#-18lx\n", actiont_type_str (type), addr, val);
      break;

   case action_typet::FREE      :
   case action_typet::MTXLOCK   :
   case action_typet::MTXUNLK   :
      printf ("%s %#-18lx\n", actiont_type_str (type), addr);
      break;

   case action_typet::THCREAT   :
   case action_typet::THSTART   :
   case action_typet::THEXIT    :
   case action_typet::THJOIN    :
      printf ("%s %u\n", actiont_type_str (type), (unsigned) val);
      break;
   }
}

