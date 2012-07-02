#include <netinet/in.h>
#include <sys/socket.h>
#include <fcntl.h>

int c_AF_INET  = AF_INET;
int c_AF_INET6 = AF_INET6;

int UnixSetNonBlocking(int Socket)
{
#if defined(O_NONBLOCK)
  int flags=fcntl(Socket,F_GETFL,0);
  if(flags=-1)
    {
      flags=0;
    }
  return fcntl(Socket, F_SETFL, O_NONBLOCK);
#else
  int flags=1;
  return ioctl(Socket,FIOBIO,&flags);
#endif
}
