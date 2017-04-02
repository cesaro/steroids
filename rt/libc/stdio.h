
#include <stdio.h>

void __rt_stdio_init ();
void __rt_stdio_term ();

FILE *_rt_fopen (const char *path, const char *mode);
FILE *_rt_fopen64 (const char *path, const char *mode);
int _rt_fclose (FILE *f);
