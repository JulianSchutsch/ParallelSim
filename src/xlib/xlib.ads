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

   function XOpenDisplay
     (display_name : Interfaces.C.Strings.chars_ptr)
      return Display_Access;
   pragma Import(C,XOpenDisplay,"XOpenDisplay");

   function DefaultScreen
     (pdf : Display_Access)
      return Interfaces.C.int;
   pragma Import(C,DefaultScreen,"_DefaultScreen");

   function XCreateWindow
     (display : Display_Access;
      parent : Window_Type;
      x : Interfaces.C.int;
      y : Interfaces.C.int;
      width : Interfaces.C.unsigned;
      height : Interfaces.C.unsigned;
      depth : Interfaces.C.int;
      class : Interfaces.C.unsigned;
      visual : Visual_Access;
      valuemask : Interfaces.C.long;
      attributes : XSetWindowAttributes_Access);

end XLib;
