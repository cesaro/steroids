
#include <err.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <steroid/steroid.h>

#include "verbosity.h"
#include "test.hh"

// we export C bindings for this whole file
extern "C"
{

struct stid
{
   const char *path;
   // ...
};

// constructor and destructor
struct stid * stid_init ()
{
   struct stid *s;
   s = (struct stid *) malloc (sizeof (struct stid));
   if (s == 0) return 0;
   s->path = 0;
   return s;
}

int stid_term (struct stid *s)
{
   free (s);
   return 0;
}

int stid_load_bytecode (struct stid *s, const char *path)
{
   printf ("load: not implemented\n");
   return 0;
}

int stid_run (struct stid *s, struct stid_replay *rep)
{
   printf ("run: not implemented\n");
   return 0;
}

int stid_get_seqexec (struct stid *s, struct stid_exec *run)
{
   ASSERT (s);
   ASSERT (run);
   da_init (&run->tab, struct stid_action);
   return 0;
}

int stid_get_poexec (struct stid *s, struct stid_po *po)
{
   return 0;
}

void stid_test ()
{
   test3 ();
}

} // extern "C"
