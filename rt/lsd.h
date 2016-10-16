
/*
 * Doubly-Linked List -- interface
 * 
 * Copyright (C) 2010 - 2016  Cesar Rodriguez <cesar.rodriguez@lsv.ens-cachan.fr>
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

#ifndef _RT_LSD_H_
#define _RT_LSD_H_

#ifdef __cplusplus
extern "C" {
#endif

struct lsd {
	struct lsd * next;
	struct lsd * prev;
};

void _rt_lsd_init (struct lsd * l);
void _rt_lsd_remove (struct lsd * l, struct lsd * p);
void _rt_lsd_insert (struct lsd * l, struct lsd *n);
void _rt_lsd_push (struct lsd * l, struct lsd *n);
int _rt_lsd_iter (struct lsd * l, int (* callback) (struct lsd * n));
void _rt_lsd_print (struct lsd * l, const char * heading);

#if 0
struct lsd * lsd_pop (struct lsd * l);
void lsd_append (struct lsd * l, struct lsd * n);
void lsd_shift (struct lsd * l);
void lsd_reverse (struct lsd * l);
void lsd_print (struct lsd * l, const char * heading);
#endif

#define lsd_item(type,nod,field) \
		((type *) ((char *) (nod) - (char *) &((type *) 0)->field))

#define lsd_i lsd_item

#ifdef __cplusplus // extern C
}
#endif

#endif

