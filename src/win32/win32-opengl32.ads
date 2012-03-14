package Win32.OpenGL32 is

   function wglCreateContext
     (hdc : HDC_Type)
      return HGLRC_Type;
   pragma Import(StdCall,wglCreateContext,"wglCreateContext");

   function wglMakeCurrent
     (hdc : HDC_Type;
      hglrc : HGLRC_Type)
      return BOOL_Type;
   pragma Import(StdCall,wglMakeCurrent,"wglMakeCurrent");

end Win32.OpenGL32;
