
void __rt_libc_init ();
void __rt_libc_term ();

void _rt_exit (int status);
void _rt__exit (int status);
int _rt_atexit (void (* fun) (void));
void _rt_abort ();
