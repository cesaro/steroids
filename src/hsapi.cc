
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
struct stid_event * stid_new_event (struct stid_action * act, unsigned int tid, unsigned int idx, unsigned int sidx)
{
   struct stid_event *e;
   e = (struct stid_event*) malloc (sizeof(struct stid_event));
   if (e == 0) return 0;
   e->act = *act;
   e->pre_mem.tid = tid;
   e->pre_mem.idx = idx;
   e->sidx = sidx;
   return e; 
} 

int stid_print_event (struct stid_event *e)
{
   if (e == 0) return 1;
   stid_print_action (&e->act);
   printf ("idx: th %u pos %u\n", e->pre_mem.tid, e->pre_mem.idx);
   //if (e->pre_proc) {
   //  // stid_print_event (e->pre_proc);
   //  printf ("pre_proc %p", e->pre_proc);
   //} else {
   //  printf ("first event in th\n");
   //}
   //if (e->pre_mem) {
   //  // stid_print_event (e->pre_mem);
   //  printf ("pre_mem %p", e->pre_mem);
   //} else {
   //  printf ("first event in mem\n");
   //}
   return 0;
}

bool stid_has_pre_proc (struct stid_event *e)
{
  return e->act.type == STID_ENTRY; 
}

bool stid_has_pre_mem (struct stid_event *e)
{
  return e->pre_mem.tid == -1;
}

struct stid_event * stid_get_pre_proc (struct stid_event *e)
{
  return e; // e->pre_proc; 
}

struct stid_event * stid_get_pre_mem (struct stid_event *e)
{
  return e; // e->pre_mem; 
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

// constructor and destructor
struct stid_po * stid_new_po ()
{
   struct stid_po *po;
   po = (struct stid_po *) malloc (sizeof (struct stid_po));
   if (po == 0) return 0;

   //da_init (&po->max_proc, struct stid_event);
   //da_init (&po->max_lock, struct stid_event);

   return po;
}

int stid_free_p (struct stid_po *s)
{
   free (s);
   return 0;
}

int stid_add_max_proc_po (struct stid_po *po, struct stid_event *e)
{
  //if (po == 0) return 1;
  //da_push (&po->max_proc, po->max_proc.len, *e, struct stid_event);
  return 0; 
}

int stid_add_max_lock_po (struct stid_po *po, struct stid_event *e)
{
  return 0;
}

} // extern "C"
