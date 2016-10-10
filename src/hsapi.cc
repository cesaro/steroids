
#include <err.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdbool.h>

#include <steroid/util/hsapi.h>

// we export C bindings for this whole file
extern "C"
{

// Testing steroid actions
struct stid_action * stid_new_action (int type, uint64_t addr, uint64_t val)
{
   struct stid_action *act;
   act = (struct stid_action*) malloc (sizeof(struct stid_action));
   if (act == 0) return 0;
   act->type = type;
   act->addr = addr;
   act->val  = val;
   return act; 
}

int stid_print_action (struct stid_action *act)
{
   if (act == 0) return 1;
   printf ("%p type %d addr %lu val %lu\n", act, act->type, act->addr, act->val);
   return 0;
}

// Testing steroid context switches
struct stid_ctsw * stid_new_ctsw (unsigned int thid, unsigned int nrev)
{
   struct stid_ctsw *ctx;
   ctx = (struct stid_ctsw*) malloc (sizeof(struct stid_ctsw));
   if (ctx == 0) return 0;
   ctx->thid = thid;
   ctx->nrev = nrev;
   return ctx; 
}

int stid_print_ctsw (struct stid_ctsw *ctx)
{
   if (ctx == 0) return 1;
   printf ("%p thid %u nrev %u \n", ctx, ctx->thid, ctx->nrev);
   return 0;
}

// Testing steroid events
struct stid_event * stid_new_event (struct stid_action * act, unsigned int th, unsigned int pos)
{
   struct stid_event *e;
   e = (struct stid_event*) malloc (sizeof(struct stid_event));
   if (e == 0) return 0;
   e->act = *act;
   e->idx.th = th;
   e->idx.pos = pos;
   e->pre_proc = 0;
   e->pre_mem = 0;
   return e; 
} 

int stid_set_pre_proc (struct stid_event *e, struct stid_event *pre_proc)
{
   if (e == 0) return 1;
   e->pre_proc = pre_proc;
   return 0;
}

int stid_set_pre_mem (struct stid_event *e, struct stid_event *pre_mem)
{
   if (e == 0) return 1;
   e->pre_mem = pre_mem;
   return 0;
}

int stid_print_event (struct stid_event *e)
{
   if (e == 0) return 1;
   stid_print_action (&e->act);
   printf ("idx: th %u pos %u\n", e->idx.th, e->idx.pos);
   if (e->pre_proc) {
     // stid_print_event (e->pre_proc);
     printf ("pre_proc %p", e->pre_proc);
   } else {
     printf ("first event in th\n");
   }
   if (e->pre_mem) {
     // stid_print_event (e->pre_mem);
     printf ("pre_mem %p", e->pre_mem);
   } else {
     printf ("first event in mem\n");
   }
   return 0;
}

bool stid_has_pre_proc (struct stid_event *e)
{
  return e->pre_proc == 0; 
}

bool stid_has_pre_mem (struct stid_event *e)
{
  return e->pre_mem == 0; 
}

struct stid_event * stid_get_pre_proc (struct stid_event *e)
{
  return e->pre_proc; 
}

struct stid_event * stid_get_pre_mem (struct stid_event *e)
{
  return e->pre_mem; 
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
