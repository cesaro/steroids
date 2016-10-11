
#include <stdio.h>
#include <assert.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <inttypes.h>
#include <ctype.h>

#if 0
#endif
long long l = 123; // 8 bytes
float f = 3.1415;
double g = 3.14151627;
long double gg = 333331321.213123;
char buff[200000];

struct cesar
{
   int  v32_0;
   char v8_1;
   int  v32_2;
};

void mystrcpy (char * dst, const char * src)
{
   while (*src)
   {
      *dst = *src;
      src++;
      dst++;
   }
   *dst = 0;
}

size_t mystrlen (const char * s)
{
   size_t i = 0;
   while (*s++) i++;
   return i;
}

void test1 (int argc, char ** argv)
{
   int x, i, j;
   unsigned char * ptr;
   printf ("test1: argc %d argv %p\n", argc, argv);

   x = argc;
   j = 0;
   ptr = (unsigned char *) &x;
   for (i = 0; i < 4; i++)
   {
      j += *ptr++;
   }

   printf ("j %d\n", j);
}

void test2 ()
{
   struct cesar c[10];
   uint64_t x[10];

   x[5] = 23;

   c[2].v32_0 = 123;
   c[2].v8_1 = 123;
   printf ("%d", c[3].v32_2);
}

int main1 (int argc, char **argv)
{
   char buff[128];
   int x = 12;
   //x -= x; // undefined behaviour !
   printf ("xxxxxxxx %d xxxxxxxxx %p xxxxxxxx\n", x, &x);
   //return 123;

   uint32_t i = 0x10121314;
   uint32_t j = 10;
   uint64_t rbp = 0x123; // current stack frame
   uint64_t rsp = 0x123; // stack pointer

   __asm__ (
      "movl %0, %1\n"
      "movq %%rbp, %2\n"
      "movq %%rsp, %3\n"
      : "+r" (i),
        "+r" (j),
        "=r" (rbp),
        "=r" (rsp));

   printf ("i %0x\n", i);
   printf ("j %0x\n", i);
   printf ("rbp %0lx\n", rbp);
   printf ("rsp %0lx\n", rsp);
   //return 7;


   printf ("argc %d &argc %p\n", argc, &argc);
   printf ("argv %p\n", argv);
   printf ("buff %p\n", buff);
   printf ("i %d &i %p\n", i, &i);
   for (x = 0; x < argc; ++x)
   {
      printf ("argv[%d] %p '%s'\n", x, argv[x], argv[x]);
   }

   printf ("malloc(4) %p\n", malloc (4));
   printf ("malloc(8) %p\n", malloc (8));
   printf ("malloc(16) %p\n", malloc (16));
   printf ("malloc(256) %p\n", malloc (256));

   return 0;

   char * ptr = buff;
   mystrcpy (ptr, "hello\n");

   printf ("calling test\n");
   test1 (argc, argv);
   printf ("test done\n");

   return argc + mystrlen (ptr);
}

int main2 ()
{
   int i;
   //int i = (int) l;
   char buff[256];

   printf ("before strcpy\n");
   mystrcpy (buff, "hello world, this is a test!!");
   printf ("after strcpy\n");
   printf ("buff %p\n", buff);
   return 111;
   for (i = 0; i < 128; i++)
   {
      printf ("i %d buff[i] %0x %c\n",
            i, buff[i], isalpha (buff[i]) ? buff[i] : '.');
   }
   return 123;
}

void test_float ()
{
   float f; // overrides symbol with same name in global scope
   double d;
   long double ld;
   printf ("test float xxxxxxxxxxxxxxxxxx\n");

   f = 3.14;
   d = 3.141526;
   ld = 2.55555555555555555555555555555555l;

   printf ("f %e\n", f);
   printf ("d %e\n", d);
   printf ("ld %.40Le\n", ld);
}

void test_mm ()
{
   unsigned char *b1;
   
   printf ("malloc 128\n");
   b1 = (unsigned char*) malloc (128);
   printf ("ret %p\n", b1);

   printf ("free %p\n", b1);
   free (b1);

   printf ("realloc (0, 256) = malloc\n");
   b1 = (unsigned char*) realloc (0, 256);
   printf ("ret %p\n", b1);

   printf ("realloc (%p, 0) = free\n", b1);
   b1 = (unsigned char*) realloc (b1, 0);
   printf ("ret %p\n", b1);

   printf ("malloc 4M\n");
   b1 = (unsigned char*) malloc (4 * 1024 * 1024);
   printf ("ret %p\n", b1);

   printf ("realloc 256M\n");
   b1 = (unsigned char*) realloc (b1, 256 * 1024 * 1024);
   printf ("ret %p\n", b1);

   printf ("free %p\n", b1);
   free (b1);
}

int main3 (int argc, char ** argv)
{
   //char buff[128];
   int i;
   uint64_t dest;
   double d1 = 3.141516;
   double d2 = 1.3333;


   printf ("buff %p\n", buff);
   mystrcpy (buff, "cesar");
   printf ("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx '%s'\n", buff);

   for (i = 0; i < argc; ++i)
   {
      printf ("argv[%d] %p '%s'\n", i, argv[i], argv[i]);
   }
   printf ("xxxxxxxxxxxxxxxxxxxx\n");
   printf ("float %zd\n", sizeof (float));
   printf ("double %zd\n", sizeof (double));
   printf ("long double %zd\n", sizeof (long double));

   dest = * ((uint64_t*) (void*) &d1);
   //dest = (uint64_t) d1;
   d2 = * ((double*) (void*) &dest);

   printf ("d1 %.8f\n", d1);
   printf ("dest %lu\n", dest);
   printf ("d2 %le\n", d2);

   test_float ();
   test_mm ();

   //f += 2;
   return 333;
}

int main4 ()
{
   int i = 0x20;
   int j = 0x33;
   int k = i + j;

   // oom for RD operation
   //k = * (int *) i;

   // oom for WR operation
   * (int *) (size_t) i = 0x8888;

   return k;
}

int main (int argc, char **argv)
{
   return main3 (argc, argv);
   //return main4 ();
}

