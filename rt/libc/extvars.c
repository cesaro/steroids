
#include <stdio.h>
#include <unistd.h>
#include <errno.h>

// stdin
extern inline FILE *__rt_var_load_stdin () {
   return stdin;
}

extern inline void __rt_var_store_stdin (FILE *f)
{
   stdin = f;
}

// stdout
extern inline FILE *__rt_var_load_stdout ()
{
   return stdout;
}

extern inline void __rt_var_store_stdout (FILE *f)
{
   stdout = f;
}

// stderr
extern inline FILE *__rt_var_load_stderr ()
{
   return stderr;
}

extern inline void __rt_var_store_stderr (FILE *f)
{
   stderr = f;
}

// optarg
extern inline char *__rt_var_load_optarg ()
{
   return optarg;
}

extern inline void __rt_var_store_optarg (char *c)
{
   optarg = c;
}

// optind
extern inline int __rt_var_load_optind ()
{
   return optind;
}

extern inline void __rt_var_store_optind (int i)
{
   optind = i;
}

// opterr
extern inline int __rt_var_load_opterr ()
{
   return opterr;
}

extern inline void __rt_var_store_opterr (int i)
{
   opterr = i;
}

// optopt
extern inline int __rt_var_load_optopt ()
{
   return optopt;
}

extern inline void __rt_var_store_optopt (int i)
{
   optopt = i;
}

extern inline char* __rt_var_load_program_invocation_name ()
{
   return program_invocation_name;
}
extern inline void __rt_var_store_program_invocation_name (char *n)
{
   program_invocation_name = n;
}
extern inline char* __rt_var_load_program_invocation_short_name ()
{
   return program_invocation_short_name;
}
extern inline void __rt_var_store_program_invocation_short_name (char *n)
{
   program_invocation_short_name = n;
}

