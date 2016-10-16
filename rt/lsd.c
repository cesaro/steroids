
/*
 * Static single-linked list -- implementation
 * 
 * Copyright (C) 2010, 2011  Cesar Rodriguez <cesar.rodriguez@lsv.ens-cachan.fr>
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "debug.h"
#include "lsd.h"

#if CONFIG_DEBUG
static void _rt_lsd_assert (struct lsd * l)
{
   struct lsd *n;

   ASSERT (l);
   if (l->prev) ASSERT (l->next);
   if (l->next) ASSERT (l->prev);

   if (l->prev) ASSERT (l->prev->prev == 0);
   if (l->next) ASSERT (l->next->next == 0);

   for (n = l->prev; n; n = n->next)
   {
      // not first
      if (n->prev)
         ASSERT (n->prev->next == n)
      else
         ASSERT (l->prev == n);

      // not last
      if (n->next)
         ASSERT (n->next->prev == n)
      else
         ASSERT (l->next == n);
   }
}
#else
#define _lsd_assert(l)
#endif

void _rt_lsd_init (struct lsd * l)
{
   l->next = 0;
   l->prev = 0;
}

void _rt_lsd_remove (struct lsd * l, struct lsd * n)
{
   /* remove node n from the list l */

   ASSERT (l->next);
   ASSERT (l->prev);

   // if n is the first or the last, we modify the list
   if (n->next == 0) l->next = n->prev;
   if (n->prev == 0) l->prev = n->next;

   // now we extract it from the list
   if (n->prev != 0) n->prev->next = n->next;
   if (n->next != 0) n->next->prev = n->prev;

   _rt_lsd_assert (l);
}

/*
Head 0x2ab15c3eaf58 first 0x2ab15c3eae80 last 0x2ab15c3eaec8 "after removing the first"
   0 nod     0x2ab15c3eae80 prev              (nil) next     0x2ab15c3eae98
   1 nod     0x2ab15c3eae98 prev     0x2ab15c3eae80 next     0x2ab15c3eaeb0
   2 nod     0x2ab15c3eaeb0 prev     0x2ab15c3eae98 next     0x2ab15c3eaec8
   3 nod     0x2ab15c3eaec8 prev     0x2ab15c3eaeb0 next              (nil)
*/
void _rt_lsd_print (struct lsd * l, const char * heading)
{
   struct lsd * n;
   int i;

   ASSERT (l); 

   printf ("Head %p first %p last %p ", l, l->prev, l->next);
   if (heading) printf ("\"%s\"", heading);

   for (n = l->prev, i = 0; n; n = n->next, i++)
   {
      printf ("\n %3d nod %18p prev %18p next %18p",
            i, n, n->prev, n->next);
   }
   printf ("\n\n");
}

void _rt_lsd_insert (struct lsd * l, struct lsd * n)
{
   /* insert the element n at the begining of the list */
   ASSERT (l);
   ASSERT (n);

   // set up the pointers in n
   n->next = l->prev;
   n->prev = 0;

   // update pointers in the first element of the list
   if (l->prev) l->prev->prev = n;

   // update pointers in l
   l->prev = n;
   if (! l->next) l->next = n;
   _rt_lsd_assert (l);
}

void _rt_lsd_push (struct lsd * l, struct lsd * n)
{
   /* insert the element n at the end of the list */
   ASSERT (l);
   ASSERT (n);

   // set up the pointers in n
   n->next = 0;
   n->prev = l->next;

   // update pointers in the last element of the list
   if (l->next) l->next->next = n;

   // update pointers in l
   l->next = n;
   if (! l->prev) l->prev = n;
   _rt_lsd_assert (l);
}

int _rt_lsd_iter (struct lsd * l, int (* callback) (struct lsd * n))
{
   struct lsd * n;
   int ret;

   ASSERT (l);

   for (n = l->prev; n; n = n->next) {
      ret = callback (n);
      if (ret < 0) return ret;
   }
   return 0;
}

#if 0
struct lsd * lsd_pop (struct lsd * l)
{
   struct lsd * n;

   /* extract and return the head of the list l, or null in case l is
    * empty */
   ASSERT (l);

   if (l->next == 0) return 0;
   n = l->next;
   l->next = l->next->next;
   _lsd_assert (l);
   return n;
}


void lsd_append (struct lsd * l, struct lsd * n)
{
   register struct lsd *tail;

   /* append the node n to the end of the list l */
   ASSERT (l);
   ASSERT (n);

   /* special case: the queue is empty */
   n->next = 0;
   if (l->next == 0) {
      l->next = n;
      return;
   }

   /* common case, the queue is not empty */
   for (tail = l->next; tail->next; tail = tail->next);
   tail->next = n;
   _lsd_assert (l);
}

void lsd_shift (struct lsd * l)
{
   struct lsd * tail;
   
   /* move the first element of the list to the last position */
   ASSERT (l);

   /* nothing to do if there is only zero or one elements */
   if (l->next == 0 || l->next->next == 0) return;

   /* search for the tail and perform the update */
   for (tail = l->next; tail->next; tail = tail->next);
   tail->next = l->next;
   l->next = l->next->next;
   tail->next = 0;
   _lsd_assert (l);
}

void lsd_reverse (struct lsd * l)
{
   struct lsd * prev;
   struct lsd * curr;
   struct lsd * next;

   /* switch the direction of the 'next' pointer in the list */
   ASSERT (l);

   /* nothing to do if there is only zero or one elements */
   if (l->next == 0) return;

   /* search for the tail and perform the update */
   prev = l->next;
   curr = prev->next;
   prev->next = 0;
   while (curr) {
      next = curr->next;
      curr->next = prev;
      prev = curr;
      curr = next;
   }
   l->next = prev;
   _lsd_assert (l);
}

void _rt_test6 ()
{
   struct lsd l;
   struct {
      int i;
      float f;
      struct lsd node;
   } nodes[10];
   int i;

   printf ("iniit list\n");

   for (i = 0; i < 10; i++)
   {
      nodes[i].i = i;
      nodes[i].f = 10.0 * i;
   }

   _rt_lsd_init (&l);
   _rt_lsd_push (&l, &nodes[0].node);
   _rt_lsd_push (&l, &nodes[1].node);
   _rt_lsd_push (&l, &nodes[2].node);
   _rt_lsd_push (&l, &nodes[3].node);
   _rt_lsd_push (&l, &nodes[4].node);
   _rt_lsd_print (&l, "original");

   _rt_lsd_remove (&l, &nodes[0].node);
   _rt_lsd_print (&l, "after removing the first");

   _rt_lsd_remove (&l, &nodes[4].node);
   _rt_lsd_print (&l, "after removing the last");

   _rt_lsd_remove (&l, &nodes[2].node);
   _rt_lsd_print (&l, "after removing one in the middle");
}

#endif
