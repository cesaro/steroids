
#define _GNU_SOURCE

#include <pthread.h>
#include <stdio.h>
#include <assert.h>
#include <unistd.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

pthread_mutex_t mut = PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP;

void *thread (void *arg)
{
   int ret;

   printf ("t1: sleeping 1 second...\n");
   sleep (1);

   ret = pthread_mutex_unlock (&mut);
   printf ("t1: unlock: ret %d\n", ret);

   ret = pthread_mutex_lock (&mut);
   printf ("t1: lock: ret %d\n", ret);
   return 0;
}

int main (int argc, char **argv)
{
   pthread_t t1;
   int ret;

   ret = pthread_mutex_lock (&mut);
   printf ("main: lock: ret %d\n", ret);
   ret = pthread_mutex_lock (&mut);
   printf ("main: lock: ret %d\n", ret);

   ret = pthread_create (&t1, 0, thread, 0);
   printf ("main: create: t1: ret %d\n", ret);

   sleep (2);
   printf ("main: unlocking...\n");
   ret = pthread_mutex_unlock (&mut);
   printf ("main: unlock: ret %d\n", ret);

   sleep (2);
   printf ("main: unlocking 2nd time...\n");
   ret = pthread_mutex_unlock (&mut);
   printf ("main: unlock: ret %d\n", ret);
   
   ret = pthread_join (t1, 0);
   printf ("main: join: t1: ret %d\n", ret);
   return 0;
}

