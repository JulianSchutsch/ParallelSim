pragma Ada_2005;

package Win32.User32 is

   function CreateWindowEx
     (dwExStyle    : DWORD_Type;
      lpClassName  : LPCTSTR_Type;
      lpWindowName : LPCTSTR_Type;
      dwStyle      : DWORD_Type;
      x            : Interfaces.C.int;
      y            : Interfaces.C.int;
      nWidth       : Interfaces.C.int;
      nHeight      : Interfaces.C.int;
      hWndParent   : HWND_Type;
      hMenu        : HMENU_Type;
      hInstance    : HINSTANCE_Type;
      lpParam      : System.Address)
      return HWND_Type;
   pragma Import(StdCall,CreateWindowEx,"CreateWindowExA");

   function DefWindowProc
     (hWnd   : HWND_Type;
      uMsg   : UINT_Type;
      wParam : WPARAM_Type;
      lParam : LPARAM_Type)
      return LResult_Type;
   pragma Import(StdCall,DefWindowProc,"DefWindowProcA");

   function SetFocus
     (hWnd : HWND_Type)
      return HWND_Type;
   pragma Import(StdCall,SetFocus,"SetFocus");

   function ShowWindow
     (hWnd     : HWND_Type;
      nCmdShow : Interfaces.C.int)
      return BOOL_Type;
   pragma Import(StdCall,ShowWindow,"ShowWindow");

   function GetDC
     (hWnd : HWND_Type)
      return HDC_Type;
   pragma Import(StdCall,GetDC,"GetDC");

   -- Replaced LONG_PTR_TYPE by LONG_Type as return type since
   -- it makes much more sense as an integer type than as a pointer
   function SetWindowLongPtr
     (hWnd      : HWND_Type;
      nIndex    : Interfaces.C.int;
      dwNewLong : LONG_PTR_Type)
      return LONG_Type;
   pragma Import(StdCall,SetWindowLongPtr,"SetWindowLongA");

   function SetForegroundWindow
     (hWnd : HWND_Type)
      return BOOL_Type;
   pragma Import(StdCall,SetForegroundWindow,"SetForegroundWindow");

   function LoadIcon
     (hInstance  : HINSTANCE_Type;
      lpIconName : LPCTSTR_Type)
      return HICON_TYPE;
   pragma Import(StdCall,LoadIcon,"LoadIconA");

   function RegisterClass
     (lpWndClass : access WNDCLASS_Type)
      return ATOM_Type;
   pragma Import(StdCall,RegisterClass,"RegisterClassA");

   function ReleaseDC
     (hWnd : HWND_Type;
      hDC  : HDC_Type)
      return Interfaces.C.int;
   pragma Import(StdCall,ReleaseDC,"ReleaseDC");

   function DestroyWindow
     (hWnd : HWND_Type)
      return BOOL_Type;
   pragma Import(StdCall,DestroyWindow,"DestroyWindow");

   function UnregisterClass
     (lpClassName : LPCTSTR_Type;
      hInstance   : HINSTANCE_Type)
      return BOOL_Type;
   pragma Import(StdCall,UnregisterClass,"UnregisterClassA");

   function GetMessage
     (lpMsg         : access MSG_Type;
      hWnd          : HWND_Type;
      wMsgFilterMin : UINT_Type;
      wMsgFilterMax : UINT_Type)
      return BOOL_Type;
   pragma Import(StdCall,GetMessage,"GetMessageA");

   function PeekMessage
     (lpMsg         : access MSG_Type;
      hWnd          : HWND_Type;
      wMsgFilterMin : UINT_Type;
      wMsgFilterMax : UINT_Type;
      wRemoveMsg    : UINT_Type)
      return BOOL_Type;
   pragma Import(StdCall,PeekMessage,"PeekMessageA");

   function PostMessage
     (hWnd   : HWND_Type;
      Msg    : UINT_Type;
      wParam : WPARAM_Type;
      lParam : LPARAM_Type)
      return BOOL_Type;
   pragma Import(StdCall,PostMessage,"PostMessageA");

   function TranslateMessage
     (lpMsg : access MSG_Type)
      return BOOL_Type;
   pragma Import(StdCall,TranslateMessage,"TranslateMessage");

   function DispatchMessage
     (lpMsg : access MSG_Type)
      return LRESULT_Type;
   pragma Import(StdCall,DispatchMessage,"DispatchMessageA");

end Win32.User32;
