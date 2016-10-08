
#include <err.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <steroid/steroid.h>

void test1 ()
{
   int i, ret;
   struct steroid *s;
   struct steroid_exec stream;
   struct steroid_action *a;

   // ctor
   s = steroid_init ();
   if (s == NULL) errx (1, "init");

   // load the program
   ret = steroid_load_bytecode (s, "/tmp/main.bc");
   if (ret != 0) errx (1, "load");

   // run
   ret = steroid_run (s, NULL);
   if (ret != 0) errx (1, "load");

   // get the stream and print it
   ret = steroid_get_seqexec (s, &stream);
   if (ret != 0) errx (1, "load");

   for (i = 0; i < stream.tab.len; i++)
   {
      a = &da_i (&stream.tab, i, struct steroid_action);
      printf ("%p type %d addr %016zx val %lu", a, a->type, a->addr, a->val);
   }
   printf ("\n");

   // dtor
   ret = steroid_term (s);
   if (ret != 0) errx (1, "term");
}
