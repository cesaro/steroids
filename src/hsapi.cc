
#include <err.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <steroid/util/hsapi.h>

// we export C bindings for this whole file
extern "C"
{

// Testing steroid actions
struct stid_action * stid_get_action()
{
   struct stid_action *act;
   act = (struct stid_action*) malloc (sizeof(struct stid_action));
   if (act == 0) return 0;
   act->type = STID_WR;
   act->addr = 0x0FFF;
   act->val = 5;
   return act; 
}

int stid_print_action(struct stid_action *act)
{
   if (act == 0) return 1;
   printf ("%p type %d addr %016zx val %lu\n", act, act->type, act->addr, act->val);
   return 0;
}

// Testing steroid context switches
struct stid_ctsw * stid_get_ctsw()
{
   struct stid_ctsw *ctx;
   ctx  = (struct stid_ctsw*) malloc (sizeof(struct stid_ctsw));
   if (ctx == 0) return 0;
   ctx->thid = 50;
   ctx->nrev = 17;
   return ctx; 
}

int stid_print_ctsw(struct stid_ctsw *ctx)
{
   if (ctx == 0) return 1;
   printf ("%p thid %u nrev %u \n", ctx, ctx->thid, ctx->nrev);
   return 0;
}

struct stid_replay * stid_get_replay()
{
   struct stid_replay * rep;
   rep = (struct stid_replay*) malloc (sizeof(struct stid_replay));
   
   int l = 0;
   struct stid_ctsw *ctx;
   ctx = (struct stid_ctsw *) malloc (sizeof(struct stid_ctsw));
   ctx->thid = 5;
   ctx->nrev = 25;
 
   da_init (&rep->tab, struct stid_ctsw);
   da_push (&rep->tab, l, *ctx, struct stid_ctsw);
   da_push (&rep->tab, l, *ctx, struct stid_ctsw);
   return rep;
}

int stid_check_replay(struct stid_replay *rep)
{
   struct stid_ctsw *ctx;

   for (int i = 0; i < rep->tab.len; i++)
   {
      ctx = &da_i (&rep->tab, i, struct stid_ctsw);
      printf ("%p thid %d nrev %d\n", ctx, ctx->thid, ctx->nrev);
   }
   printf ("\n");
   return 0;
}

} // extern "C"
