
#include <err.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <steroid/steroid.h>

void test0 ()
{
   stid_test ();
}

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

int main (int argc, char **argv)
{
   test0 ();
   return 0;
}

