#include "X11/Xlib.h"

int _DefaultScreen(Display * display)
{
  return DefaultScreen(display);
}

Window _RootWindow(Display * display, int screen)
{
  return RootWindow(display,screen);
}
