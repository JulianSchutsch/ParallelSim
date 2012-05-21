#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>

int geterrno()
{
  return errno;
}

int _exec(const char * filename, char *arguments)
{
  return execl(filename,arguments,NULL);
}

void SetNonBlocking(int file)
{
  int flags=fcntl(file,F_GETFL,0);
  if (flags=-1)
  {
    flags=0;
  }
  fcntl(file,F_SETFL,flags | O_NONBLOCK);
}
