
#define _GNU_SOURCE

#include <pthread.h>
#include <stdio.h>
#include <assert.h>
#include <unistd.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
//#include <sys/time.h>
//#include <sys/resource.h>

pthread_mutex_t mut = PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP;

void *start1 (void *arg)
{
   printf ("t1: sleeping 2 second...\n");
   sleep (2);

   printf ("t1: sleeping 1 more second...\n");
   sleep (1);

   return 0;
}

int main1 ()
{
   pthread_t t1;
   int ret;

   ret = pthread_create (&t1, 0, start1, 0);
   printf ("main: create: t1: ret %d\n", ret);

   //ret = pthread_detach (t1);
   //printf ("main: detach: t1: ret %d\n", ret);

   sleep (1);
   printf ("main: returning!\n");


   // the application will terminate if main returns or calls exit(3)
   //return 0;

   // but it will wait if we call pthread_exit, with either detached or
   // non-detached threads
   pthread_exit (0);

   return 0;
}

int main (int argc, char **argv)
{
   return main1 ();
}

