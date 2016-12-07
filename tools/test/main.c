#include <err.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <stid/c/steroid.h>
#include <stid/c/util/hsapi.h>

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
   int r = stid_action_print (a);
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
   struct stid_event *c = stid_new_event (a, 1, 3, 1);
   int r = stid_event_print (c, 1, 1);
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
   struct stid_po *po = stid_example_po ();

   stid_po_print (po);
}

void test9 ()
{
   struct stid *s;
   struct stid_po *po;
   const char *path = "input.ll";
   struct stid_replay replay;
   int ret;

   s = stid_init ();
   if (s == 0) errx (1, "init failed");

   // load the program
   ret = stid_load_bytecode (s, path);
   if (ret != 0) errx (1, "load failed");

   // prepare arguments for the program
   stid_argv_add (s, "cunf");
   stid_argv_add (s, "/tmp/dme3.ll_net");

   // prepare some replay
   da_init (&replay.tab, struct stid_ctsw);
   da_trunc (&replay.tab, 3, struct stid_ctsw);

   da_i (&replay.tab, 0, struct stid_ctsw).thid = 0;
   da_i (&replay.tab, 0, struct stid_ctsw).nrev = 3; 

   da_i (&replay.tab, 1, struct stid_ctsw).thid = 2;
   da_i (&replay.tab, 1, struct stid_ctsw).nrev = 1; 

   da_i (&replay.tab, 2, struct stid_ctsw).thid = 1;
   da_i (&replay.tab, 2, struct stid_ctsw).nrev = 1; 

   // run, with replay sequence
   ret = stid_run (s, &replay);
   if (ret != 0) errx (1, "run failed");

   // print partial order
   po = stid_po_get (s);
   if (po == 0) errx (1, "get_po failed");

   // print it
   stid_po_print (po);

#if 0
   // run the guest
   //std::vector<int> replay2 {0, 5, 2, 1, 1, 4, 0, 1, 2, 3, 0, 2, -1};
   //std::vector<int> replay2 {0, 3,  2, 1,  1, 1,  0, 2, -1};
   std::vector<int> replay2 {0, 5,  1, 1,  2, 1,  1, 3, -1};
#endif
}

int main (int argc, char **argv)
{
   //test9 ();
   stid_test ();
   return 0;
}
