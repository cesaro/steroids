
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

static inline const char *act_ty_to_str (int a)
{
   switch (a)
   {
   // threads
   case STID_CREATE : return "STID_CREATE";
   case STID_JOIN   : return "STID_JOIN  ";
   case STID_ENTRY  : return "STID_ENTRY ";
   case STID_EXIT   : return "STID_EXIT  ";
   // locks
   case STID_LOCK   : return "STID_LOCK  ";
   case STID_UNLOCK : return "STID_UNLOCK";
   default : return "FAIL";
   }
}

int stid_print_action (struct stid_action *act)
{
   if (act == 0) return 1;
   printf ("action: %p type %s addr %#lx val %#lx\n", 
      act, act_ty_to_str(act->type), act->addr, act->val);
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

int stid_print_event (struct stid_event *e, unsigned int tid, unsigned int idx)
{
   if (e == 0) return 1;

   printf ("eventt this %18p sidx %5d tid %2d pos %4u ac.type %s pre_mem { tid %4d idx %4d }\n",
         e,
         e->sidx,
         tid,
         idx,
         act_ty_to_str (e->act.type),
         e->pre_mem.tid,
         e->pre_mem.idx);

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

// Testing replay
struct stid_replay * stid_get_replay()
{
   struct stid_replay * rep;
   rep = (struct stid_replay *) malloc (sizeof(struct stid_replay));

   da_init (&rep->tab, struct stid_ctsw);
   da_trunc (&rep->tab, 2, struct stid_ctsw);

   da_i (&rep->tab, 0, struct stid_ctsw).thid = 1;
   da_i (&rep->tab, 0, struct stid_ctsw).nrev = 3; 

   da_i (&rep->tab, 1, struct stid_ctsw).thid = 10;
   da_i (&rep->tab, 1, struct stid_ctsw).nrev = 250; 

//   int l = 0;
//   struct stid_ctsw *ctx;
//   ctx = (struct stid_ctsw *) malloc (sizeof(struct stid_ctsw));
//   ctx->thid = 5;
//   ctx->nrev = 25;
// 
//   da_init (&rep->tab, struct stid_ctsw);
//   da_push (&rep->tab, l, *ctx, struct stid_ctsw);
//   da_push (&rep->tab, l, *ctx, struct stid_ctsw);

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

   return po;
}

int stid_free_po (struct stid_po *s)
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

int stid_print_po (struct stid_po *po)
{
   struct da *p;
   struct stid_event *e;
   struct stid_event *e1;

   printf ("Begin Events\n------------------\n");

   for (int tid = 0; tid < po->procs.len; tid++)
   {
      p = &da_i (&po->procs, tid, struct da);
      e = &da_i (p, 0, struct stid_event); 
      e1 = &da_i (p, p->len-1, struct stid_event); 
      printf ("Thread %d, size %u, first %p, last %p\n", 
            tid, p->len, e, e1);

      for (int tlen = 0; tlen < p->len; tlen++)
      {
        stid_print_event (e++, tid, tlen);
      }
   }

   return 0; 
}

int stid_print_seq_po (struct stid_po *po)
{
   // current stream position
   int spos = 0;
   bool end_stream = false;
   bool fixpoint;
   int rpos = 0;

   // create the frontier per process 
   struct stid_event* proc_pos[po->procs.len];
   struct stid_event* proc_ipos[po->procs.len];
   struct da *p;
   struct stid_event* e;

   for (int tid = 0; tid < po->procs.len; tid++)
   {
     p = &da_i (&po->procs, tid, struct da);
     proc_pos[tid] = &da_i (p, 0, struct stid_event); 
     proc_ipos[tid] = &da_i (p, 0, struct stid_event); 
   } 

   printf ("Begin Events\n------------------\n");

   while (!end_stream) 
   {
     fixpoint = true;
     for (int tid = 0; tid < po->procs.len; tid++)
     {
       e = proc_pos[tid];
       if (e->sidx == spos)
       {
         rpos = e - proc_ipos[tid];
         stid_print_event (e, tid, rpos);
         proc_pos[tid]++;
         spos++;
         fixpoint = false; 
       }
     }
     if (fixpoint) 
       end_stream = true;
   }

   return 0; 
}

struct stid_po * stid_example_po () 
{
   struct stid_po *po;
   po = (struct stid_po *) malloc (sizeof (struct stid_po));
   if (po == 0) return 0;

   struct da *p0;
   struct da *p1;

   da_init (&po->procs, struct da);
   da_trunc (&po->procs, 2, struct da); // 2 processes

   // process 1, 4 events
   p0 = &da_i(&po->procs, 0, struct da);
   da_init (p0, struct stid_event);
   da_trunc (p0, 6, struct stid_event);

   // p0, event 0, entry (this is bottom!!)
   da_i (p0, 0, struct stid_event).act.type = STID_ENTRY;
   da_i (p0, 0, struct stid_event).act.addr = 0; // irrelevant
   da_i (p0, 0, struct stid_event).act.val = 0; // irrelevant
   da_i (p0, 0, struct stid_event).pre_mem.tid = -1; // no pre_memdecessor other than in the thread (this is main!)
   da_i (p0, 0, struct stid_event).sidx = 0;

   // p0, event 1, create
   da_i (p0, 1, struct stid_event).act.type = STID_CREATE;
   da_i (p0, 1, struct stid_event).act.addr = 0; // irrelevant
   da_i (p0, 1, struct stid_event).act.val = 1; // we create thread 1
   da_i (p0, 1, struct stid_event).pre_mem.tid = -1;
   da_i (p0, 1, struct stid_event).sidx = 1;

   // p0, event 2, lock
   da_i (p0, 2, struct stid_event).act.type = STID_LOCK;
   da_i (p0, 2, struct stid_event).act.addr = 0xfffff000;
   da_i (p0, 2, struct stid_event).act.val = 0; // irrelevant
   da_i (p0, 2, struct stid_event).pre_mem.tid = 1; // pre_mem = thread 1, event 2 (the unlock)
   da_i (p0, 2, struct stid_event).pre_mem.idx = 2;
   da_i (p0, 2, struct stid_event).sidx = 5;

   // p0, event 3, unlock
   da_i (p0, 3, struct stid_event).act.type = STID_UNLOCK;
   da_i (p0, 3, struct stid_event).act.addr = 0xfffff000;
   da_i (p0, 3, struct stid_event).act.val = 0; // irrelevant
   da_i (p0, 3, struct stid_event).pre_mem.tid = 0; // pre_mem (last lock in this process)
   da_i (p0, 3, struct stid_event).pre_mem.idx = 2;
   da_i (p0, 3, struct stid_event).sidx = 6;

   // p0, event 4, exit
   da_i (p0, 4, struct stid_event).act.type = STID_JOIN;
   da_i (p0, 4, struct stid_event).act.val = 1; // join of thread 1
   da_i (p0, 4, struct stid_event).pre_mem.tid = 1; // pre_mem = thread one's exit event
   da_i (p0, 4, struct stid_event).pre_mem.idx = 3;
   da_i (p0, 4, struct stid_event).sidx = 8;

   // p0, event 5, exit
   da_i (p0, 5, struct stid_event).act.type = STID_EXIT;
   da_i (p0, 5, struct stid_event).sidx = 9;

   // process 2, 2 events
   p1 = &da_i(&po->procs, 1, struct da);
   da_init (p1, struct stid_event);
   da_trunc (p1, 4, struct stid_event);

   // p1, event 0, entry
   da_i (p1, 0, struct stid_event).act.type = STID_ENTRY;
   da_i (p1, 0, struct stid_event).pre_mem.tid = 0; // pre_mem = thread 0, event 1 (the create)
   da_i (p1, 0, struct stid_event).pre_mem.idx = 1;
   da_i (p1, 0, struct stid_event).sidx = 2;

   // p1, event 1, lock
   da_i (p1, 1, struct stid_event).act.type = STID_LOCK;
   da_i (p1, 1, struct stid_event).act.addr = 0xffffff00;
   da_i (p1, 1, struct stid_event).act.val = 0; // irrelevant
   da_i (p1, 1, struct stid_event).pre_mem.tid = -1; // beginning of this address total order
   da_i (p1, 1, struct stid_event).sidx = 3;

   // p1, event 2, unlock
   da_i (p1, 2, struct stid_event).act.type = STID_UNLOCK;
   da_i (p1, 2, struct stid_event).act.addr = 0xffffff00;
   da_i (p1, 2, struct stid_event).pre_mem.tid = 1; // pre_mem (last lock in this process)
   da_i (p1, 2, struct stid_event).pre_mem.idx = 1;
   da_i (p1, 2, struct stid_event).sidx = 4;

   // p0, event 4, exit
   da_i (p1, 3, struct stid_event).act.type = STID_EXIT;
   da_i (p1, 3, struct stid_event).sidx = 7;

   // max lock struct: testing with non lock events 
   da_init (&po->max_lock, struct stid_event);
   da_trunc (&po->max_lock, 2, struct stid_event);

   da_i (&po->max_lock, 0, struct stid_event).act.type = STID_ENTRY;
   da_i (&po->max_lock, 0, struct stid_event).act.addr = 0; // irrelevant
   da_i (&po->max_lock, 0, struct stid_event).act.val = 0; // irrelevant
   da_i (&po->max_lock, 0, struct stid_event).pre_mem.tid = -1; // no pre_memdecessor other than in the thread (this is main!)
   da_i (&po->max_lock, 0, struct stid_event).sidx = 0;

   da_i (&po->max_lock, 1, struct stid_event).act.type = STID_ENTRY;
   da_i (&po->max_lock, 1, struct stid_event).act.addr = 0; // irrelevant
   da_i (&po->max_lock, 1, struct stid_event).act.val = 0; // irrelevant
   da_i (&po->max_lock, 1, struct stid_event).pre_mem.tid = -1; // no pre_memdecessor other than in the thread (this is main!)
   da_i (&po->max_lock, 1, struct stid_event).sidx = 0;
 
   return po;
}

} // extern "C"
