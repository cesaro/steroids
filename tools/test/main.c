#include <err.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <steroid/steroid.h>
#include <steroid/util/hsapi.h>

// Various tests for the API

// Test the entire pipeline 
void test1 ()
{
   int i, ret;
   struct stid *s;
   struct stid_exec stream;
   struct stid_action *a;

   // ctor
   s = stid_init ();
   if (s == NULL) errx (1, "init");

   // load the program
   ret = stid_load_bytecode (s, "/tmp/main.bc");
   if (ret != 0) errx (1, "load");

   // run
   ret = stid_run (s, NULL);
   if (ret != 0) errx (1, "load");

   // get the stream and print it
   ret = stid_get_seqexec (s, &stream);
   if (ret != 0) errx (1, "load");

   for (i = 0; i < stream.tab.len; i++)
   {
      a = &da_i (&stream.tab, i, struct stid_action);
      printf ("%p type %d addr %016zx val %lu", a, a->type, a->addr, a->val);
   }
   printf ("\n");

   // dtor
   ret = stid_term (s);
   if (ret != 0) errx (1, "term");
}

// Create and print an action
void test2 () 
{
   struct stid_action *a = stid_new_action (STID_WR, 0xFF, 5);
   int r = stid_print_action (a);
   printf ("ret code %d\n", r);
}

// Create and print a context
void test3 () 
{
   struct stid_ctsw *c = stid_new_ctsw (1, 2);
   int r = stid_print_ctsw (c);
   printf ("ret code %d\n", r);
}

// Create and print an event 
void test4 () 
{
   struct stid_action *a = stid_new_action (STID_WR, 0xFF, 5);
   struct stid_event *c = stid_new_event (a, 1, 3);
   int r = stid_print_event (c);
   printf ("ret code %d\n", r);
}

// Create and iterate a dummy execution
void test5 () 
{
   struct stid_exec run;
   struct stid_action *a;
   int l = 0;
   struct stid_action el;
   el.type = STID_RD;
   el.addr = 0x0F;
   el.val = 5;
   
   da_init (&run.tab, struct stid_action);
   da_push (&run.tab, l, el, struct stid_action);
   el.val = 6; 
   da_push (&run.tab, l, el, struct stid_action); 

   for (int i = 0; i < run.tab.len; i++)
   {
      a = &da_i (&run.tab, i, struct stid_action);
      printf ("%p type %d addr %016zx val %lu\n", a, a->type, a->addr, a->val);
   }
   printf ("\n");
}

// Create a dummy replay
void test6 ()
{
   struct stid_replay * rep;
   rep = stid_get_replay();
   int r = stid_check_replay (rep);
   printf ("ret code %d\n", r);
}

// Create and check a replay 
void test7 ()
{
   struct stid_replay rep;
   int l = 0;
   struct stid_ctsw ctx;
   ctx.thid = 1;
   ctx.nrev = 1;
 
   da_init (&rep.tab, struct stid_ctsw);
   da_push (&rep.tab, l, ctx, struct stid_ctsw);
   ctx.nrev = 2; 
   da_push (&rep.tab, l, ctx, struct stid_ctsw);

   int r = stid_check_replay (&rep);
   printf ("ret code %d\n", r);
}

void test8 ()
{
#if 0


#endif

   struct stid_po po;
   struct da *p0;
   struct da *p1;

   da_init (&po.procs, struct da);
   da_trunc (&po.procs, 2, struct da); // 2 processes

   // process 1, 4 events
   p0 = &da_i(&po.procs, 0, struct da);
   da_init (p0, struct stid_event);
   da_trunc (p0, 4, struct stid_event);

   // p0, event 0, entry (this is bottom!!)
   da_i (p0, 0, struct stid_event).act.type = STID_ENTRY;
   da_i (p0, 0, struct stid_event).act.addr = 0; // irrelevant
   da_i (p0, 0, struct stid_event).act.val = 0; // irrelevant
   da_i (p0, 0, struct stid_event).mempre.tid = -1; // no mempredecessor other than in the thread (this is main!)
   da_i (p0, 0, struct stid_event).sidx = 0;

   // p0, event 1, create
   da_i (p0, 1, struct stid_event).act.type = STID_CREATE;
   da_i (p0, 1, struct stid_event).act.addr = 0; // irrelevant
   da_i (p0, 1, struct stid_event).act.val = 1; // we create thread 1
   da_i (p0, 1, struct stid_event).mempre.tid = -1;
   da_i (p0, 1, struct stid_event).sidx = 1;

   // p0, event 2, lock
   da_i (p0, 2, struct stid_event).act.type = STID_LOCK;
   da_i (p0, 2, struct stid_event).act.addr = 0xfffff000;
   da_i (p0, 2, struct stid_event).act.val = 0; // irrelevant
   da_i (p0, 2, struct stid_event).mempre.tid = 1; // mempre = thread 1, event 2 (the unlock)
   da_i (p0, 2, struct stid_event).mempre.idx = 2;
   da_i (p0, 2, struct stid_event).sidx = 5;

   // p0, event 3, unlock
   da_i (p0, 3, struct stid_event).act.type = STID_UNLOCK;
   da_i (p0, 3, struct stid_event).act.addr = 0xfffff000;
   da_i (p0, 3, struct stid_event).act.val = 0; // irrelevant
   da_i (p0, 3, struct stid_event).mempre.tid = 0; // mempre (last lock in this process)
   da_i (p0, 3, struct stid_event).mempre.idx = 2;
   da_i (p0, 3, struct stid_event).sidx = 6;

   // p0, event 4, exit
   da_i (p0, 4, struct stid_event).act.type = STID_JOIN;
   da_i (p0, 4, struct stid_event).act.val = 1; // join of thread 1
   da_i (p0, 4, struct stid_event).mempre.tid = 1; // mempre = thread one's exit event
   da_i (p0, 4, struct stid_event).mempre.idx = 3;
   da_i (p0, 4, struct stid_event).sidx = 8;

   // p0, event 5, exit
   da_i (p0, 5, struct stid_event).act.type = STID_EXIT;
   da_i (p0, 5, struct stid_event).sidx = 9;

   

   // process 2, 2 events
   p1 = &da_i(&po.procs, 1, struct da);
   da_init (p1, struct stid_event);
   da_trunc (p1, 2, struct stid_event);

   // p1, event 0, entry
   da_i (p1, 0, struct stid_event).act.type = STID_ENTRY;
   da_i (p1, 0, struct stid_event).mempre.tid = 0; // mempre = thread 0, event 1 (the create)
   da_i (p1, 0, struct stid_event).mempre.idx = 1;
   da_i (p1, 0, struct stid_event).sidx = 2;

   // p1, event 1, lock
   da_i (p1, 1, struct stid_event).act.type = STID_LOCK;
   da_i (p1, 1, struct stid_event).act.addr = 0xffffff00;
   da_i (p1, 1, struct stid_event).act.val = 0; // irrelevant
   da_i (p1, 1, struct stid_event).mempre.tid = -1; // beginning of this address total order
   da_i (p1, 1, struct stid_event).sidx = 3;

   // p1, event 2, unlock
   da_i (p1, 2, struct stid_event).act.type = STID_UNLOCK;
   da_i (p1, 2, struct stid_event).act.addr = 0xffffff00;
   da_i (p1, 2, struct stid_event).mempre.tid = 1; // mempre (last lock in this process)
   da_i (p1, 2, struct stid_event).mempre.idx = 1;
   da_i (p1, 2, struct stid_event).sidx = 4;

   // p0, event 4, exit
   da_i (p1, 3, struct stid_event).act.type = STID_EXIT;
   da_i (p1, 3, struct stid_event).sidx = 7;
}

int main (int argc, char **argv)
{
   test4 ();
   //stid_test ();
   return 0;
}
b
