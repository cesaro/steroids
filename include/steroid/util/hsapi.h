
#ifndef _STEROID_UTIL_HSAPI_H_
#define _STEROID_UTIL_HSAPI_H_

#ifdef __cplusplus
extern "C" {
#endif

#include <steroid/steroid.h>

// testing steroid actions
struct stid_action * stid_new_action (int, uint64_t, uint64_t);

int stid_print_action (struct stid_action *);

// testing steroid context switches
struct stid_ctsw * stid_new_ctsw (unsigned int, unsigned int);

int stid_print_ctsw (struct stid_ctsw *ctx);

// testing steroid replay
struct stid_replay * stid_get_replay ();

int stid_check_replay (struct stid_replay *rep);

#ifdef __cplusplus
} // extern "C"
#endif

#endif // _STEROID_UTIL_HSAPI_H_
