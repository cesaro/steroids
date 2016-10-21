
#include <sys/types.h>
#include <stdlib.h>
#include <stdio.h>

void __VERIFIER_error()
{
ERROR:
   printf ("VERIFIER_error executed!!!!\n");
   fflush (stdout);
   fflush (stderr);
   abort();
   goto ERROR;
}

void __attribute__((weak)) __VERIFIER_assert(int expr)
{
   if (! expr) __VERIFIER_error ();
}

void __VERIFIER_assume(int expression)
{
   if (!expression)
   {
      while (1);
   }
   return;
}

_Bool      __VERIFIER_nondet_bool ()
{
   return 0;
}

char      __VERIFIER_nondet_char ()
{
   return 0;
}

int       __VERIFIER_nondet_int ()
{
   return 0;
}

float     __VERIFIER_nondet_float ()
{
   return 0;
}

double    __VERIFIER_nondet_double ()
{
   return 0;
}

loff_t    __VERIFIER_nondet_loff_t ()
{
   return 0;
}

long      __VERIFIER_nondet_long ()
{
   return 0;
}

pthread_t __VERIFIER_nondet_pthread_t ()
{
   return 0;
}

short     __VERIFIER_nondet_short ()
{
   return 0;
}

size_t    __VERIFIER_nondet_size_t ()
{
   return 0;
}

unsigned  __VERIFIER_nondet_unsigned ()
{
   return 0;
}

ushort    __VERIFIER_nondet_ushort ()
{
   return 0;
}

#if 0
pchar     __VERIFIER_nondet_pchar ()
{
   return 0;
}

pointer   __VERIFIER_nondet_pointer ()
{
   return 0;
}

sector_t  __VERIFIER_nondet_sector_t ()
{
   return 0;
}

u32       __VERIFIER_nondet_u32 ()
{
   return 0;
}

uchar     __VERIFIER_nondet_uchar ()
{
   return 0;
}

uint      __VERIFIER_nondet_uint ()
{
   return 0;
}

ulong     __VERIFIER_nondet_ulong ()
{
   return 0;
}
#endif

