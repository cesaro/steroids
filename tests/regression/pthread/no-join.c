
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void *thread (void *arg)
{
   printf ("t1: running\n");
   printf ("t1: returning\n");
   return 0;
}

int main (int argc, char **argv)
{
   pthread_t t1;
   int ret;

   ret = pthread_create (&t1, 0, thread, 0);
   printf ("main: create: t1: ret %d\n", ret);

   pthread_exit (0);
}

