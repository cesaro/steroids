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

// Create and iterate a dummy execution
void test4 () 
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

void test5 ()
{
   struct stid_replay * rep;
   rep = stid_get_replay();
   int r = stid_check_replay (rep);
   printf ("ret code %d\n", r);
}

void test6 ()
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

int main (int argc, char **argv)
{
   test3();
   //stid_test ();
   return 0;
}

