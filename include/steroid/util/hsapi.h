
#ifndef _STEROID_UTIL_HSAPI_H_
#define _STEROID_UTIL_HSAPI_H_

#ifdef __cplusplus
extern "C" {
#endif

#include <stdbool.h>
#include <steroid/steroid.h>

// testing steroid actions
struct stid_action * stid_new_action (int, uint64_t, uint64_t);
int stid_print_action (struct stid_action *);

// testing steroid context switches
struct stid_ctsw * stid_new_ctsw (unsigned int, unsigned int);
int stid_print_ctsw (struct stid_ctsw *);

// testing steroid events
struct stid_event * stid_new_event (struct stid_action *, unsigned int, unsigned int, unsigned int);
int stid_set_pre_proc (struct stid_event *, struct stid_event *);
int stid_set_pre_mem  (struct stid_event *, struct stid_event *);
int stid_print_event (struct stid_event *); 
bool stid_has_pre_proc (struct stid_event *);
bool stid_has_pre_mem (struct stid_event *);
struct stid_event * stid_get_pre_proc(struct stid_event *);
struct stid_event * stid_get_pre_mem(struct stid_event *);

// testing steroid execution

// testing steroid replay
struct stid_replay * stid_get_replay ();
int stid_check_replay (struct stid_replay *);

// testing steroid po
struct stid_po * stid_new_po ();
int stid_free_po (struct stid_po *);
int stid_add_max_proc_po (struct stid_po *, struct stid_event *);
int stid_add_max_lock_po (struct stid_po *, struct stid_event *);

#ifdef __cplusplus
} // extern "C"
#endif

#endif // _STEROID_UTIL_HSAPI_H_
