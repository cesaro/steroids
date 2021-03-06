
#include <stdio.h>
#include <assert.h>
#include <unistd.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <inttypes.h>
#include <ctype.h>
#include <pthread.h>
#include <sched.h>
#include <fcntl.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/stat.h>

#if 0
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
   mystrcpy (buff, "Hello world, this is a test!!");
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

void *main5_thread (void *arg)
{
   int i = * (int *) arg;
   printf ("t%d: running, pid %d\n", i, getpid ());
   usleep (1000000);
   printf ("t%d: after!\n", i);
   return 0;
}

void my_pthread_create (pthread_t *tid, void *(*start) (void *), void *arg)
{
   void *ptr;
   pthread_attr_t attr;
   int ret;

#define S (2 << 20)

   // allocate a 2M stack
   ptr = malloc (S); // 2M
   printf ("ptr %p\n", ptr);

   // define thread attributes
   ret = pthread_attr_init (&attr);
   printf ("attr_init %d\n", ret);
   //ret = pthread_attr_setstack (&attr, ptr, S);
   //printf ("attr_setstack %d\n", ret);
   //ret = pthread_attr_setstacksize (&attr, 13 << 20);
   //printf ("attr_setstacksize %d\n", ret);

#undef S

   ret = pthread_create (tid, &attr, start, arg);
   printf ("pthread_create: ret %d\n", ret);
}

int main5 ()
{
   pthread_t tid1;
   pthread_t tid2;
   int id1 = 1;
   int id2 = 2;
   int ret;
   struct timespec ts;
   struct sched_param param;

   // print round robin time interval
   ret = sched_rr_get_interval (getpid (), &ts);
   printf ("sched_rr_get_interval: ret %d\n", ret);
   printf ("sec %lu\n", ts.tv_sec);
   printf ("nsec %lu\n", ts.tv_nsec);

   // set scheduling to SCHED_FIFO !
   int prio;
   prio = getpriority (PRIO_PROCESS, getpid ());
   printf ("getpriority %d\n", prio);
   printf ("sched_get_priority_min %d\n", sched_get_priority_min (SCHED_FIFO));
   printf ("sched_get_priority_max %d\n", sched_get_priority_max (SCHED_FIFO));
   ret = sched_getparam (0, &param);
   printf ("sched_getparam: ret %d; prio %d\n", ret, param.sched_priority);

   param.sched_priority = 1;
   ret = sched_setscheduler (0, SCHED_FIFO, &param);
   printf ("sched_setscheduler: ret %d '%s'\n", ret, strerror (errno));
   printf ("sched_setscheduler: ret %d\n", ret);

   // create 2 threads
   my_pthread_create (&tid1, main5_thread, &id1);
   my_pthread_create (&tid2, main5_thread, &id2);

   ret = pthread_join (tid1, 0);
   printf ("pthread_join: ret %d\n", ret);

   ret = pthread_join (tid2, 0);
   printf ("pthread_join: ret %d\n", ret);

   return 0;
}

void *thread6 (void *arg)
{
   (void) arg;
   printf ("thread: sleeping 400ms ...\n");
   usleep (400000);
   printf ("thread: exiting!\n");
   //exit (123);
   //pthread_exit ((void*) (long) 0x11223344);
   return (void*) (long) 0x11223344;
}

int main6 ()
{
   pthread_t t;
   int ret;
   void *retval;

   ret = pthread_create (&t, 0, thread6, 0);
   printf ("main: pthread_create: ret %d\n", ret);

   //pthread_exit ((void*) (long) 0x11223344);

   ret = pthread_join (t, &retval);
   printf ("main: pthread_join: ret %d retval %p\n", ret, retval);

   printf ("main: bye bye !\n");
   pthread_exit (0);
}

void *thread7 (void *arg)
{
   int i = (unsigned) arg;
   int ms;

   printf ("thread%d: starting! arg %p\n", i, arg);
   //ms = 1000 + random () % 200;
   ms = 200 + random () % 200;
   printf ("thread%d: sleeping %dms ...!\n", i, ms);
   usleep (ms * 1000);
   printf ("thread%d: exiting!\n", i);
   return (void*) (long) 10 + i;
}

#define NR7 2

int main7 ()
{
   pthread_t t[NR7];
   int ret, i;
   void *retval[NR7];

   for (i = 0; i < NR7; i++)
   {
      ret = pthread_create (t + i, 0, thread7, (void *) (long) i);
      printf ("main: pthread_create: ret %d\n", ret);
   }

   for (i = 0; i < NR7; i++)
   {
      ret = pthread_join (t[i], retval + i);
      printf ("main: pthread_join: ret %d retval %p\n", ret, retval[i]);
   }

   printf ("main: bye bye !\n");
   return 5656;
}

pthread_mutex_t m8 = PTHREAD_MUTEX_INITIALIZER;

void *thread8 (void *arg)
{
   int i = (unsigned) arg;
   int ms, ret;

   // sleep
   printf ("thread%d: starting! arg %p\n", i, arg);
   ms = 100 + random () % 500;
   printf ("thread%d: sleeping %dms ...!\n", i, ms);
   usleep (ms * 1000);

   // lock, unlock
   ret = pthread_mutex_lock (&m8);
   printf ("thread%d: mutex_lock %d!\n", i, ret);
   ret = pthread_mutex_unlock (&m8);
   printf ("thread%d: mutex_unlock %d!\n", i, ret);

   printf ("thread%d: exiting!\n", i);
   return (void*) (long) 10 + i;
}

#define NR8 2

int main8 ()
{
   pthread_t t[NR8];
   int ret, i;
   void *retval[NR8];

   // create
   for (i = 0; i < NR8; i++)
   {
      ret = pthread_create (t + i, 0, thread8, (void *) (long) i);
      printf ("main: pthread_create: ret %d\n", ret);
   }

   // lock, unlock
   ret = pthread_mutex_lock (&m8);
   printf ("main: mutex_lock %d!\n", ret);
   ret = pthread_mutex_unlock (&m8);
   printf ("main: mutex_unlock %d!\n", ret);

   // join
   for (i = 0; i < NR8; i++)
   {
      ret = pthread_join (t[i], retval + i);
      printf ("main: pthread_join: ret %d retval %p\n", ret, retval[i]);
   }

   close (1);
   printf ("main: bye bye !\n");
   return 5656;
}

uint64_t words[128];
uint64_t *wordsptr = words;

static inline void instr (void *addr, int size)
{
   int i;
   unsigned char *src = (unsigned char *) addr;

#if 0
   unsigned char *dst = (unsigned char *) wordsptr;
   for (i = 0; i < size; i++) dst[i] = src[i];
   wordsptr += size / 8;
   if (size % 8) wordsptr++;
#else
   uint64_t *dst = wordsptr;
   for (i = 0; i + 8 <= size; i += 8)
   {
      *dst++ = * (uint64_t*) (src + i);
   }

   switch (size - i)
   {
   case 1 :
      *dst++ = * (uint8_t*) (src + i);
      break;
   case 2 :
      *dst++ = * (uint16_t*) (src + i);
      break;
   case 3 :
      *dst++ = * (uint16_t*) (src + i);
      *dst++ = * (uint8_t*)  (src + i + 2);
      break;
   case 4 :
      *dst++ = * (uint32_t*) (src + i);
      break;
   case 5 :
      *dst++ = * (uint32_t*) (src + i);
      *dst++ = * (uint8_t*)  (src + i + 4);
      break;
   case 6 :
      *dst++ = * (uint32_t*) (src + i);
      *dst++ = * (uint16_t*) (src + i + 4);
      break;
   case 7 :
      *dst++ = * (uint32_t*) (src + i);
      *dst++ = * (uint16_t*) (src + i + 4);
      *dst++ = * (uint8_t*)  (src + i + 5);
      break;
   }
   wordsptr = dst;
#endif
}

int main9 ()
{
   float x1, x2;
   long double y;

   printf ("give me two numbers: \n");
   fflush (stdout);
   scanf ("%f%f", &x1, &x2);

   y = x1 + x2 + 123;

   instr (&y, sizeof (long double));

   return 0;
}


pthread_mutex_t m10 = PTHREAD_MUTEX_INITIALIZER;

void *thread10 (void *arg)
{
   int i;

   (void) arg;

   for (i = 0; i < 5; i++)
   {
      pthread_mutex_lock (&m10);
      printf ("t: in critical section!!!!!\n");
      pthread_mutex_unlock (&m10);
   }

   return NULL;
}

int main10 ()
{
   pthread_t t;
   int ret;

   /* create threads */
   ret = pthread_create (&t, NULL, thread10, NULL);
   //ret = pthread_create (&t, NULL, thread10, NULL);
   printf ("m: create: ret %d\n", ret);
   assert (ret == 0);

   thread10 (0);
   sleep (1);

   pthread_exit (0);
   return 0;
}

pthread_mutex_t m11 = PTHREAD_MUTEX_INITIALIZER;

void *thread11 (void *arg)
{
   long j = (long) arg;
   int i;

   for (i = 0; i < 3; i++)
   {
      pthread_mutex_lock (&m11);
      printf ("t%ld: in critical section!!!!!\n", j);
      pthread_mutex_unlock (&m11);
   }

   return NULL;
}

int main11 ()
{
   pthread_t t;
   int ret;

   /* create threads */
   ret = pthread_create (&t, NULL, thread11, (void*) 1);
   printf ("m: create: ret %d\n", ret);
   assert (ret == 0);
   ret = pthread_create (&t, NULL, thread11, (void*) 2);
   printf ("m: create: ret %d\n", ret);
   assert (ret == 0);

   thread11 (0);
   //sleep (1);

   pthread_exit (0);
   return 0;
}

int __thread a = 0x11223344;
int __thread b = 0x55667788;
struct abc {
   int x;
   float f;
};
struct abc __thread st = {10, 3.1483294320};
int __thread vec[30] = {0x12, 0x13, 0x14};
int __thread c;
extern int __thread fuera;
long double __thread f1;
int __thread c2;

int main12 (int argc, char **argv)
{
   static uint64_t __thread main12x = 112233;

   int x[5] = {0, 1, 2, 3, 4};

   (void) argv;

   if (argc)
   {
      x[2] += 10;
   }
   else
   {
      x[3] += 20;
      a = 10;
      b = 20;
      st.x = 123;
      st.f = 8899;
      main12x = 123123;
   }

   printf ("x[2] %d x[3] %d %f %lu\n", x[2], x[3], st.f, main12x);

   uint64_t l;

   l = (uint64_t) &st;
   l += 10;
   l = l < 23;
   c = strlen ((void*) l);
   a = 123;
   printf ("%d\n", c);

   return 0;
}

int main13 (int argc, char **argv)
{
   (void) argv;

   c = 10;
   printf ("hello 13\n");
   printf ("argc %d\n", argc);
   printf ("argv %p\n", argv);
   printf ("a %x\n", a);
   printf ("b %x\n", b);
   return argc + st.x + c;
}

int __thread tls14 = 123;

void *thread14 (void *arg)
{
   (void) arg;

   printf ("m: tls14 %d &tls14 %p\n", tls14, &tls14);
   tls14 = 222;
   printf ("m: tls14 %d &tls14 %p (after)\n", tls14, &tls14);
   return NULL;
}

int main14 ()
{
   pthread_t t;
   int ret;


   printf ("m: tls14 %d &tls14 %p\n", tls14, &tls14);
   tls14 = 111;
   printf ("m: tls14 %d &tls14 %p (after)\n", tls14, &tls14);

   /* create threads */
   ret = pthread_create (&t, NULL, thread14, (void*) 1);
   printf ("m: create: ret %d\n", ret);
   assert (ret == 0);


   printf ("m: tls14 %d &tls14 %p\n", tls14, &tls14);
   tls14 = 111;
   printf ("m: tls14 %d &tls14 %p (after)\n", tls14, &tls14);

   pthread_exit (0);
}
#endif

int main15 ()
{
   int fd;
   int ret, ret2;
   char buff[1024];

   fd = open ("/tmp/1ped.fasta", O_RDONLY);
   printf ("fd %d\n", fd);

   ret = read (fd, buff, 20);
   printf ("ret %d\n", ret);

   ret2 = write (1, buff, ret);
   printf ("ret2 %d\n", ret2);

   ret = fwrite (buff, 10, 2, stdout);

   ret = fclose (stdout);
   printf ("ret %d\n", ret);

   FILE *f;

   f = fopen ("/tmp/x.fasta", "r");
   printf ("f %p\n", f);

   ret = close (fd);
   printf ("ret %d\n", ret);

   ret = sleep (2);
   printf ("ret %d errno %d\n", ret, errno);

   ret = usleep (800 * 1000);
   printf ("ret %d errno %d\n", ret, errno);

   f = fdopen (1, "w");
   printf ("f %p\n", f);

   ret = fclose (f);
   printf ("ret %d\n", ret);

   printf ("herreeeeeeeeeeeeeeeeeeee\n");

   return 0;
}


void main16 ()
{
   //int x = "hello world"[3];
   //int y;
   //x = 123;
   //y = x;

   //int x;
   //int y;
   //int *p;

   //if (x)
   //   p = &x;
   //else
   //   p = &y;
   //*p = 123;
   //x = 123;
   //return 0;
}

int main (int argc, char **argv)
//int main ()
{
   (void) argc;
   (void) argv;

   //return main9 ();
   //return main10 ();
   return main15 ();
   //return main13 (argc, argv);
}
