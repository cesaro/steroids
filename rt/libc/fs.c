
#include <sys/types.h>
#include <fcntl.h>
#include <stdarg.h>

#include "fs.h"

int _rt_open (const char *path, int flag, ...)
{
   va_list ap;
   mode_t mode;
   int ret;

   va_start (ap, flag);
   mode = va_arg (ap, int); // third argument
   va_end (ap);

   ret = open (path, flag, mode);
   STRACE (fs, "open (path='%s', flags=%x, mode=0%o) = %d",
      path, flag, mode, ret);
   return ret;
}

int _rt_close (int fd)
{
   int ret;

   if (fd == 0 || fd == 1 || fd == 2)
   {
      STRACE (fs, "close (fd=%d) = 0", fd);
      ALERT ("fd %d: refraining to close %d, but returning success\n", fd, fd);
      return 0; // fake !
   }
   else
   {
      ret = close (fd);
      STRACE (fs, "close (fd=%d) = %d", fd, ret);
      return ret;
   }
}

ssize_t _rt_read (int fd, void *buff, size_t count)
{
   ssize_t ret;
   ret = read (fd, buff, count);
   STRACE (fs, "read (fd=%d, buff=%p, count=%zu) = %zd",
         fd, buff, count, ret);
   return ret;
}

ssize_t _rt_write (int fd, void *buff, size_t count)
{
   ssize_t ret;
   ret = write (fd, buff, count);
   STRACE (fs, "write (fd=%d, buff=%p, count=%zu) = %zd",
         fd, buff, count, ret);
   return ret;
}
