
#include <err.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <steroid/steroid.h>

// we export C bindings for this whole file
extern "C"
{

struct steroid
{
   const char *path;
   // ...
};

// constructor and destructor
struct steroid * steroid_init ()
{
   struct steroid *s;
   s = (struct steroid *) malloc (sizeof (struct steroid));
   if (s == 0) return 0;
   s->path = 0;
   return s;
}

int steroid_term (struct steroid *s)
{
   free (s);
   return 0;
}

int steroid_load_bytecode (struct steroid *s, const char *path)
{
   printf ("load: not implemented\n");
   return 0;
}

int steroid_run (struct steroid *s, struct steroid_replay *rep)
{
   printf ("run: not implemented\n");
   return 0;
}

int steroid_get_seqexec (struct steroid *s, struct steroid_exec *run)
{
   //ASSERT (s);
   //ASSERT (run);
   da_init (&run->tab, struct steroid_action);
   return 0;
}

int steroid_get_poexec (struct steroid *s, struct steroid_po *po)
{
   return 0;
}

} // extern "C"
