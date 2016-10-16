
#ifndef _RT_DEBUG_H_
#define _RT_DEBUG_H_

#include <stdio.h>
#include <stdlib.h>

#define CONFIG_DEBUG 1
//#undef CONFIG_DEBUG

void _rt_breakme (void);

#define BREAK(expr) if (expr) breakme ()
#define PRINT(fmt,args...) \
      printf (__FILE__ ":%d: %s: " fmt "\n", __LINE__, __func__, ##args)

#ifdef CONFIG_DEBUG
#define ASSERT(expr) \
	{if (! (expr)) { \
		PRINT (__FILE__ ":%d: %s: Assertion `" #expr "' failed.\n", \
				__LINE__, __func__); \
		_rt_breakme (); \
		exit (1); \
	}}
#define DEBUG(fmt,args...) PRINT (fmt,##args)
#else
#define ASSERT(expr)
#define DEBUG(fmt,args...)
#endif

#define DEBUG2(fmt,args...) \
	PRINT (__FILE__ ":%d: %s: " fmt, __LINE__, __func__, ##args)
#define SHOW(expr,type)	DEBUG (#expr "='%" type "'", expr)

#endif
