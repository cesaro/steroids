
#undef NDEBUG

#include <vector>

#include <err.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <assert.h>
#include <string.h>

#include <stid/c/steroid.h>

// Various tests for the API

struct opts
{
   const char *path;
   std::vector<int> replay;
   std::vector<const char *> args;
   int limit_stream_print;
};

void usage ()
{
   printf (
R"XX(stid BYTECODE [ARGS] [-limit LIMIT] [-replay REPLAY]

BYTECODE is the instrumented bytecode
ARGS     is zero or more commandline arguments: default 1 arg, the BYTECODE file
LIMIT    is a the maximum number of events in the stream to print; default: all
REPLAY   is a sequence of numbers in the replay format; default empty
)XX");
   exit (1);
}

void parse_opts (int argc, const char ** argv, struct opts *opts)
{
   int i;

   assert (opts);

   // invocation error
   if (argc < 2) usage ();

   // default values
   opts->path = argv[1];
   opts->replay.clear();
   opts->args.clear();
   opts->limit_stream_print = 0;

   // arguments
   for (i = 2; i < argc; i++)
   {
      if (strcmp (argv[i], "-limit") == 0) break;
      if (strcmp (argv[i], "-replay") == 0) break;
      opts->args.push_back (argv[i]);
   }

   // limit
   if (i < argc and strcmp (argv[i], "-limit") == 0)
   {
      if (i + 1 >= argc) usage();
      opts->limit_stream_print = (int) strtoul (argv[i+1], 0, 10);
      i += 2;
   }
   if (i < argc and strcmp (argv[i], "-replay") == 0)
   {
      i++;
      for (; i < argc; i++)
      {
         opts->replay.push_back ((int) strtoul (argv[i], 0, 10));
         assert (opts->replay.back() >= 0);
      }
   }
   assert ((opts->replay.size() & 0x1) == 0); 
   if (i != argc) usage ();

   // default argv
   if (opts->args.size() == 0) opts->args.push_back (opts->path);

   printf ("opts.bytecode : '%s'\n", opts->path);
   printf ("opts.limit    : %d\n", opts->limit_stream_print);
   printf ("opts.args     : ");
   for (const char *arg : opts->args) printf ("'%s' ", arg);
   printf ("\nopts.replay   : ");
   for (int i : opts->replay) printf ("%d ", i);
   printf ("\n");
}

void replay2replay (std::vector<int> &rep1, struct stid_replay &rep2)
{
   struct stid_ctsw *cs;

   if (rep1.size() == 0) return;

   da_init (&rep2.tab, struct stid_ctsw);
   da_trunc (&rep2.tab, rep1.size() / 2, struct stid_ctsw);

   for (unsigned i = 0; i < rep1.size() - 1; i += 2)
   {
      assert (i + 1 < rep1.size());
      cs = &da_i(&rep2.tab, i/2, struct stid_ctsw);
      cs->thid = rep1[i];
      cs->nrev = rep1[i+1];
   }
}

void main1 (struct opts *opts)
{
   struct stid_handle *s;
   struct stid_po *po;
   struct stid_replay replay;
   int ret;

   s = stid_init ();
   if (s == 0) errx (1, "init failed");

   // load the program
   ret = stid_load_bytecode (s, opts->path);
   if (ret != 0) errx (1, "load failed");

   // prepare arguments for the program
   for (const char *arg : opts->args) stid_argv_add (s, arg);

   // prepare some replay
   replay2replay (opts->replay, replay);

   // run, with or without replay sequence
   ret = stid_run (s, opts->replay.size() == 0 ? 0 : &replay);
   if (ret != 0) errx (1, "run failed");

   // aha!
   stid_cmd (s, STID_CMD1, &opts->limit_stream_print, 0, 0);

   // print partial order
   po = stid_po_get (s);
   if (po == 0) errx (1, "po-get failed");
   stid_po_print (po);

   // deallocate the po
   stid_po_term (po);

   fflush (stdout);
   fflush (stderr);
}

int main (int argc, const char **argv)
{
   struct opts opts;
   parse_opts (argc, argv, &opts);
   main1 (&opts);
   return 0;
}

