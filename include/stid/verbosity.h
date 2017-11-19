#ifndef __STID_VERBOSITY_H_
#define __STID_VERBOSITY_H_

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>

#include "stid/c/config.h"

#ifdef __cplusplus
extern "C" {
#endif

// the different levels of verbosity in the program
#define VERB_DEBUG   3
#define VERB_TRACE   2
#define VERB_INFO    1
#define VERB_PRINT   0

// setting and getting the verbosity level
void verb_set (int i);
int verb_get ();

#ifdef __cplusplus
} // extern "C"
#endif // __cplusplus

#endif // __STID_VERBOSITY_H_
