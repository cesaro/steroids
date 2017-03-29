
#include <stdlib.h>

#include "stdio.h"

#define MAX_STDIO_FILES 256
static struct {
   FILE* tab[MAX_STDIO_FILES]; // list of currently open files
   unsigned size;
} __stdio_state;

static void __rt_stdio_add (FILE *f)
{
   // store the file f in one of the slots in the table of currently open files
   if (__stdio_state.size >= MAX_STDIO_FILES)
   {
      ALERT ("Cannot open a new file: hard limit of %d reached", MAX_STDIO_FILES);
      abort();
   }
   __stdio_state.tab[__stdio_state.size] = f;
   __stdio_state.size++;
}

static void __rt_stdio_remove (FILE *f)
{
   // remove f from the table of open files

   unsigned i;

   for (i = 0; i < __stdio_state.size; i++)
   {
      if (__stdio_state.tab[i] == f)
      {
         if (i != __stdio_state.size - 1)
         {
            __stdio_state.tab[i] = __stdio_state.tab[__stdio_state.size - 1];
         }
         __stdio_state.size--;
         return;
      }
   }
   ASSERT (0);
}

void __rt_stdio_init ()
{
   __stdio_state.size = 0;
}

void __rt_stdio_term ()
{
   
   // close all files left open by the program
   if (__stdio_state.size)
      INFO ("stid: rt: stdio: closing now %u files left open by the program",
            __stdio_state.size);
   while (__stdio_state.size)
      _rt_fclose (__stdio_state.tab[0]);
}

FILE *_rt_fopen (const char *path, const char *mode)
{
   FILE *f;

   f = fopen (path, mode);
   if (f) __rt_stdio_add (f);
   STRACE (fs, "fopen (path='%s', mode='%s') = %p", path, mode, f);
   return f;
}


FILE *_rt_fdopen(int fd, const char *mode)
{
   FILE *f;

   // This switch avoids to reopening descriptors 0, 1, or 2 as new streams and
   // then closing them with fclose(), as we will correctly detect there that
   // they are stdin, stdout, or stderr. This is just another quick hack ...
   switch (fd) {
   case 0 : f = stdin; break;
   case 1 : f = stdout; break;
   case 2 : f = stderr; break;
   default :
      f = fdopen (fd, mode);
      if (f) __rt_stdio_add (f);
   }
   STRACE (fs, "fdopen (fd=%d, mode='%s') = %p", fd, mode, f);
   return f;
}

FILE *_rt_freopen(const char *path, const char *mode, FILE *f)
{
   FILE *newf;
   // BUG: f may not be open, so calling __rt_stdio_remove() will produce an
   // unnecessary assertion violation
   __rt_stdio_remove (f);
   newf = freopen (path, mode, f);
   if (newf) __rt_stdio_add (newf);
   STRACE (fs, "freopen (path='%s', mode='%s', old=%p) = %p", path, mode, f, newf);
   return newf;
}

int _rt_fclose (FILE *f)
{
   int ret;
   const char *n =
         f == stdout ? "stdout" :
         f == stdin ? "stdin" :
         f == stderr ? "stderr" : "?";

   if (f == stdout || f == stdin || f == stderr)
   {
      STRACE (fs, "fclose (f=%s) = 0", n);
      ALERT ("f %p (%s): refraining to close %s, but returning success", f, n, n);
      return 0; // fake!
   }
   else
   {
      __rt_stdio_remove (f);
      ret = fclose (f);
      STRACE (fs, "fclose (f=%p) = %d", f, ret);
      return ret;
   }
}

size_t _rt_fread (void *ptr, size_t size, size_t n, FILE *f)
{
   size_t ret;
   ret = fread (ptr, size, n, f);
   STRACE (fs, "fread (buff=%p, size=%zu, n=%zu, f=%p) = %zu",
         ptr, size, n, f, ret);
   return ret;
}

size_t _rt_fwrite (const void *ptr, size_t size, size_t n, FILE *f)
{
   size_t ret;
   ret = fwrite (ptr, size, n, f);
   STRACE (fs, "fwrite (buff=%p, size=%zu, n=%zu, f=%p) = %zu",
         ptr, size, n, f, ret);
   return ret;
}

// man stdio(3) shows the list of all functions that we could put here

