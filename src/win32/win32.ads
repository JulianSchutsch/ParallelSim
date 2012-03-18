pragma Ada_2005;

with Interfaces;
with Interfaces.C;
with Interfaces.C.Strings;
with System;

package Win32 is

   pragma Elaborate_Body;

   type UINT_Type is new Interfaces.Unsigned_32;
   type DWORD_Type is new Interfaces.Unsigned_32;
   type HANDLE_Type is new Interfaces.C.ptrdiff_t;
   type UINT_PTR_Type is new Interfaces.C.ptrdiff_t;
   subtype LONG_PTR_Type is System.Address;
   type LONG_Type is new Interfaces.C.ptrdiff_t;
   type HWND_Type is new HANDLE_Type;
   type WPARAM_Type is new HWND_Type;
   subtype LPARAM_Type is LONG_PTR_Type;
   type HINSTANCE_Type is new HANDLE_Type;
   type HICON_Type is new HANDLE_Type;
   subtype HCURSOR_Type is HICON_Type;
   type HBRUSH_Type is new HANDLE_Type;
   subtype LPCTSTR_Type is Interfaces.C.Strings.chars_ptr;
   type WORD_Type is new Interfaces.Unsigned_16;
   type ATOM_Type is new WORD_Type;
   type HMENU_Type is new HANDLE_TYPE;
   type LRESULT_Type is new LONG_Type;
   type HDC_Type is new HANDLE_Type;
   type BOOL_Type is new Interfaces.C.int;
   type BYTE_Type is new Interfaces.Unsigned_8;
   type HGLRC_Type is new HANDLE_Type;

   TRUE  : constant BOOL_Type:=1;
   FALSE : constant BOOL_Type:=0;

   function MAKEINTRESOURCE
     (wInteger : WORD_Type)
      return LPCTSTR_Type;

   CS_HREDRAW : constant UINT_Type:=2;
   CS_VREDRAW : constant UINT_Type:=1;
   CS_OWNDC   : constant UINT_Type:=32;

   WS_OVERLAPPEDWINDOW : constant DWORD_Type:=16#cf0000#;
   WS_CLIPCHILDREN     : constant DWORD_Type:=16#2000000#;
   WS_CLIPSIBLINGS     : constant DWORD_Type:=16#4000000#;
   WS_EX_APPWINDOW     : constant DWORD_Type:=16#40000#;
   WS_EX_CLIENTEDGE    : constant DWORD_Type:=512;

   WM_MOUSELEAVE    : constant:=16#2A3#;
   WM_PAINT         : constant:=15;
   WM_CREATE        : constant:=1;
   WM_SIZE          : constant:=5;
   WM_SIZING        : constant:=532;
   WM_LBUTTONDBLCLK : constant:=515;
   WM_RBUTTONDBLCLK : constant:=518;
   WM_LBUTTONDOWN   : constant:=513;
   WM_LBUTTONUP     : constant:=514;
   WM_RBUTTONDOWN   : constant:=517;
   WM_RBUTTONUP     : constant:=516;
   WM_MOUSEMOVE     : constant:=512;
   WM_DESTROY       : constant:=2;
   WM_ERASEBKGND    : constant:=20;
   WM_KEYDOWN       : constant:=256;
   WM_KEYUP         : constant:=257;
   WM_CHAR          : constant:=258;
   WM_TIMER         : constant:=275;
   WM_QUIT          : constant:=18;

   SW_SHOW : constant := 5;

   PM_NOREMOVE : constant := 0;
   PM_REMOVE   : constant := 1;
   PM_NOYIELD  : constant := 2;

   PFD_DRAW_TO_WINDOW : constant:=4;
   PFD_SUPPORT_OPENGL : constant:=32;
   PFD_TYPE_RGBA      : constant:=0;
   PFD_MAIN_PLANE     : constant:=0;

   type WNDPROC_Access is
     access function
       (hWnd    : HWND_Type;
        message : UINT_Type;
        wParam  : WPARAM_Type;
        lParam  : LPARAM_Type)
        return LRESULT_Type;
   pragma Convention(C,WNDPROC_Access);

   type WNDCLASS_Type is
      record
         style         : UINT_Type:=0;
         lpfnWndProc   : WNDPROC_Access:=null;
         cbClsExtra    : Interfaces.C.int:=0;
         cbWndExtra    : Interfaces.C.int:=0;
         hInstance     : HINSTANCE_Type:=0;
         hIcon         : HICON_Type:=0;
         hCursor       : HCURSOR_Type:=0;
         hbrBackground : HBRUSH_Type:=0;
         lpszMenuName  : LPCTSTR_Type:=Interfaces.C.Strings.Null_Ptr;
         lpszClassName : LPCTSTR_Type:=Interfaces.C.Strings.Null_Ptr;
      end record;
   pragma Convention(C,WNDCLASS_Type);

   type CREATESTRUCT_Type is
      record
         lpCreateParams : System.Address;
         hInstance      : HINSTANCE_Type;
         hMenu          : HMENU_Type;
         hwndParent     : HWND_Type;
         cy             : Interfaces.C.int;
         cx             : Interfaces.C.int;
         y              : Interfaces.C.int;
         x              : Interfaces.C.int;
         style          : LONG_Type;
         lpszName       : LPCTSTR_Type;
         lpszClass      : LPCTSTR_Type;
         dwExStyle      : DWORD_Type;
      end record;
   type CREATESTRUCT_Access is access CREATESTRUCT_Type;
   pragma Convention(C,CREATESTRUCT_Type);

   type POINT_Type is
      record
         x : LONG_Type;
         y : LONG_Type;
      end record;

   type MSG_Type is
      record
         hwnd : HWND_Type;
         message : UINT_Type;
         wParam : UINT_Type;
         lParam : UINT_Type;
         time : DWORD_Type;
         pt : POINT_Type;
      end record;
   pragma Convention(C,MSG_Type);

   type PIXELFORMATDESCRIPTOR_Type is
      record
         nSize           : WORD_Type:=0;
         nVersion        : WORD_Type:=0;
         dwFlags         : DWORD_Type:=0;
         iPixelType      : BYTE_Type:=0;
         cColorBits      : BYTE_Type:=0;
         cRedBits        : BYTE_Type:=0;
         cRedShift       : BYTE_Type:=0;
         cGreenBits      : BYTE_Type:=0;
         cGreenShift     : BYTE_Type:=0;
         cBlueBits       : BYTE_Type:=0;
         cBlueShift      : BYTE_Type:=0;
         cAlphaBits      : BYTE_Type:=0;
         cAlphaShift     : BYTE_Type:=0;
         cAccumBits      : BYTE_Type:=0;
         cAccumRedBits   : BYTE_Type:=0;
         cAccumGreenBits : BYTE_Type:=0;
         cAccumBlueBits  : BYTE_Type:=0;
         cAccumAlphaBits : BYTE_Type:=0;
         cDepthBits      : BYTE_Type:=0;
         cStencilBits    : BYTE_Type:=0;
         cAuxBuffers     : BYTE_Type:=0;
         iLayerType      : BYTE_Type:=0;
         bReserved       : BYTE_Type:=0;
         dwLayerMask     : DWORD_Type:=0;
         dwVisibleMask   : DWORD_Type:=0;
         dwDamageMask    : DWORD_Type:=0;
      end record;
   pragma Convention(C,PIXELFORMATDESCRIPTOR_Type);

   function GetLastError
     return DWORD_TYPE;
   pragma Import(StdCall,GetLastError,"GetLastError");

end Win32;
