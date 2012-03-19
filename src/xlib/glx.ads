with Xlib; use Xlib;
with OpenGL; use OpenGL;
with Interfaces.C;

package glX is

   GLX_RGBA         : constant:=4;
   GLX_RED_SIZE     : constant:=8;
   GLX_GREEN_SIZE   : constant:=9;
   GLX_BLUE_SIZE    : constant:=10;
   GLX_DOUBLEBUFFER : constant:=5;
   GLX_DEPTH_SIZE   : constant:=12;
   GLX_NONE         : constant:=101;

   -- PORTABILITY : Boolean may not be defined as expected, working
   --               on Debian Sqeeze so far

   function glXQueryVersion
     (dpy   : Display_Access;
      major : GLint_Access;
      minor : GLint_Access)
      return Interfaces.C.int;
   pragma Import(C,glXQueryVersion,"glXQueryVersion");

   function glXChooseVisual
     (dpy : Display_access;
      screen : Interfaces.C.int;
      attribList : access Interfaces.C.int)
      return XVisualInfo_Access;
   pragma Import(C,glXChooseVisual,"glXChooseVisual");


end glX;
