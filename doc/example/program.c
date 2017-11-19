#include <stdio.h>
#include <pthread.h>
#include <string.h>
#include <assert.h>

int counter = 0;

pthread_mutex_t m = PTHREAD_MUTEX_INITIALIZER;

void *thread (void *arg)
{
   (void) arg;

   pthread_mutex_lock (&m);
   printf ("thread: incrementing\n");
   counter++;
   pthread_mutex_unlock (&m);

   return 0;
}

int main (int argc, char ** argv)
{
   pthread_t th;

   (void) argc;
   (void) argv;

   pthread_create (&th, 0, thread, 0);

   pthread_mutex_lock (&m);
   printf ("main: checking\n");
   if (counter == 0) counter = 1;
   pthread_mutex_unlock (&m);

   pthread_join (th, 0);

   assert (counter == 2); // can be violated, if thread 1 runs first

   return 0;
}
