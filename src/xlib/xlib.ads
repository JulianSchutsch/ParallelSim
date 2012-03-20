pragma Ada_2005;
with Interfaces.C.Strings;

package Xlib is

   type XID_Type is new Interfaces.C.long;
   type XID_Access is access all XID_Type;

   type Window_Type is new XID_Type;
   type Window_Access is access all Window_Type;

   type Display_Type is null record;
   type Display_Access is access Display_Type;

   type Visual_Type is null record;
   type Visual_Access is access all Visual_Type;

   type VisualID_Type is new Interfaces.C.long;
   type VisualID_Access is access all VisualID_Type;

   type ColorMap_Type is new XID_Type;
   type ColorMap_Access is access all ColorMap_Type;

   type Pixmap_Type is new XID_Type;
   type Pixmap_Access is access all Pixmap_Type;

   type Cursor_Type is new XID_Type;
   type Cursor_Access is access all Cursor_Type;

   type Atom_Type is new Interfaces.C.long;
   type Atom_Access is access all Atom_Type;

   type XVisualInfo_Type is
      record
         visual        : Visual_Access;
         visualid      : VisualID_Type;
         screen        : Interfaces.C.int;
         depth         : Interfaces.C.int;
         class         : Interfaces.C.int;
         red_mask      : Interfaces.C.long;
         green_mask    : Interfaces.C.long;
         blue_mask     : Interfaces.C.long;
         colormap_size : Interfaces.C.int;
         bits_per_rgb  : Interfaces.C.int;
      end record;
   pragma Convention(C,XVisualInfo_Type);
   type XVisualInfo_Access is access all XVisualInfo_Type;

   StructureNotifyMask : constant := 2**17;
   ExposureMask        : constant := 2**15;
   KeyPressMask        : constant := 2**0;
   KeyReleaseMask      : constant := 2**1;
   ButtonPressMask     : constant := 2**2;
   ButtonReleaseMask   : constant := 2**3;
   PointerMotionMask   : constant := 2**6;

   type XSetWindowAttributes_Type is
      record
         background_pixmap    : Pixmap_Type:=0;
         background_pixel     : Interfaces.C.long:=0;
         border_pixmap        : Pixmap_Type:=0;
         border_pixel         : Interfaces.C.long:=0;
         bit_gravity          : Interfaces.C.int:=0;
         win_gravity          : Interfaces.C.int:=0;
         backing_store        : Interfaces.C.int:=0;
         backing_planes       : Interfaces.C.long:=0;
         backing_pixel        : Interfaces.C.long:=0;
         save_under           : Interfaces.C.int:=0;
         event_mask           : Interfaces.C.long:=0;
         do_not_propagate_ask : Interfaces.C.long:=0;
         override_redirect    : Interfaces.C.int:=0;
         colormap             : Colormap_Type:=0;
         cursor               : Cursor_Type:=0;
      end record;
   pragma Convention(C,XSetWindowAttributes_Type);
   type XSetWindowAttributes_Access is access all XSetWindowAttributes_Type;

   type Aspect_Type is
      record
         x : Interfaces.C.int:=0;
         y : Interfaces.C.int:=0;
      end record;
   pragma Convention(C,Aspect_Type);

   USSize     : constant:=2**1;
   USPosition : constant:=2**0;

   type XSizeHints_Type is
      record
         flags       : Interfaces.C.long:=0;
         x           : Interfaces.C.int:=0;
         y           : Interfaces.C.int:=0;
         width       : Interfaces.C.int:=0;
         height      : Interfaces.C.int:=0;
         min_width   : Interfaces.C.int:=0;
         min_height  : Interfaces.C.int:=0;
         max_width   : Interfaces.C.int:=0;
         max_height  : Interfaces.C.int:=0;
         width_inc   : Interfaces.C.int:=0;
         height_inc  : Interfaces.C.int:=0;
         min_aspect  : Aspect_Type;
         max_ascpect : Aspect_Type;
         base_width  : Interfaces.C.int:=0;
         base_height : Interfaces.C.int:=0;
         win_gravity : Interfaces.C.int:=0;
      end record;
   pragma Convention(C,XSizeHints_Type);
   type XSizeHints_Access is access all XSizeHints_Type;

   function XOpenDisplay
     (display_name : Interfaces.C.Strings.chars_ptr)
      return Display_Access;
   pragma Import(C,XOpenDisplay,"XOpenDisplay");

   function DefaultScreen
     (pdf : Display_Access)
      return Interfaces.C.int;
   pragma Import(C,DefaultScreen,"_DefaultScreen");

   function RootWindow
     (display : Display_Access;
      screen  : Interfaces.C.int)
      return Window_Type;
   pragma Import(C,RootWindow,"_RootWindow");

   InputOutput   : constant := 1;
   CWBackPixel   : constant := 2**1;
   CWBorderPixel : constant := 2**3;
   CWColorMap    : constant := 2**13;
   CWEventMask   : constant := 2**11;

   function XCreateWindow
     (display      : Display_Access;
      parent       : Window_Type;
      x            : Interfaces.C.int;
      y            : Interfaces.C.int;
      width        : Interfaces.C.unsigned;
      height       : Interfaces.C.unsigned;
      border_width : Interfaces.C.int;
      depth        : Interfaces.C.int;
      class        : Interfaces.C.unsigned;
      visual       : Visual_Access;
      valuemask    : Interfaces.C.long;
      attributes   : XSetWindowAttributes_Access)
      return Window_Type;
   pragma Import(C,XCreateWindow,"XCreateWindow");

   AllocNone : constant:=0;

   function XCreateColormap
     (display : Display_Access;
      window  : Window_Type;
      visual  : Visual_Access;
      alloc   : Interfaces.C.int)
      return ColorMap_Type;
   pragma Import(C,XCreateColorMap,"XCreateColormap");

   procedure XSetNormalHints
     (display : Display_Access;
      window  : Window_Type;
      hints   : XSizeHints_Access);
   pragma Import(C,XSetNormalHints,"XSetNormalHints");

   procedure XSetStandardProperties
     (display     : Display_Access;
      window      : Window_Type;
      window_name : Interfaces.C.Strings.chars_ptr;
      icon_name   : Interfaces.C.Strings.chars_ptr;
      icon_pixmap : Pixmap_Type;
      argv        : access Interfaces.C.Strings.chars_ptr;
      argc        : Interfaces.C.int;
      hints       : XSizeHints_Access);
   pragma Import(C,XSetStandardProperties,"XSetStandardProperties");

   function XInternAtom
     (display        : Display_Access;
      atom_name      : Interfaces.C.Strings.chars_ptr;
      only_if_exists : Interfaces.C.int)
      return Atom_Type;
   pragma Import(C,XInternAtom,"XInternAtom");

end XLib;
