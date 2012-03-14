pragma Ada_2005;

package Win32.GDI32 is

   function ChoosePixelFormat
     (hdc  : HDC_Type;
      ppfd : access PIXELFORMATDESCRIPTOR_Type)
      return Interfaces.C.int;
   pragma Import(StdCall,ChoosePixelFormat,"ChoosePixelFormat");

   function SetPixelFormat
     (hdc          : HDC_Type;
      iPixelFormat : Interfaces.C.int;
      ppfd         : access PIXELFORMATDESCRIPTOR_Type)
      return BOOL_Type;
   pragma Import(StdCall,SetPixelFormat,"SetPixelFormat");

end Win32.GDI32;
