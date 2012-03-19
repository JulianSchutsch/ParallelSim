#include "X11/Xlib.h"

int _DefaultScreen(Display * display)
{
  return DefaultScreen(display);
}
