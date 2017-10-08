
#ifndef _RT_DEBUG_H_
#define _RT_DEBUG_H_

#include <stdio.h>
#include <stdlib.h>

//#define CONFIG_DEBUG 1
#undef CONFIG_DEBUG

// synonym of printf
#define PRINT_(fmt,args...) printf (fmt, ##args)
#define PRINT(fmt,args...)  PRINT_ (fmt "\n", ##args)

// important messages to the final user, usually regarding assumptions of the
// implementation
#define ALERT(fmt,args...) \
   PRINT (__FILE__ ":%d: %s: " fmt, __LINE__, __func__, ##args)

// messages displayed only if rt->flags.verbose is enabled
#define INFO(fmt,args...) \
   { if (rt->flags.verbose) PRINT (fmt, ##args); }

// assertion checking + DEBUG macro
#ifdef CONFIG_DEBUG
#define ASSERT(expr) \
	do { \
      if (! (expr)) { \
         ALERT ("Assertion `" #expr "' failed.\n"); \
         breakme (); \
         fflush (stdout); \
         exit (1); \
	   } \
   } while (0)
#define DEBUG(fmt,args...) PRINT (fmt,##args)
#else
#define ASSERT(expr) do {} while (0)
#define DEBUG(fmt,args...) do {} while (0)
#endif

// DEBUG macro that remains enabled when we disable CONFIG_DEBUG
#define DEBUG2(fmt,args...) \
	PRINT (__FILE__ ":%d: %s: " fmt, __LINE__, __func__, ##args)

// the great and only SHOW macro :)
#define SHOW(expr,type)	DEBUG (#expr "='%" type "'", expr)

// gdb debugging
void breakme (void);
#define BREAK(expr) if (expr) breakme ()

#endif
