
unsigned int _rt_sleep (unsigned int sec);
int _rt_usleep (useconds_t us);
int _rt_clock_nanosleep (clockid_t id, int flags, const struct timespec *req, struct timespec *rem);
int *_rt___errno_location ();
void _rt___assert_fail (const char *__assertion, const char *__file,
			   unsigned int __line, const char *__function);

