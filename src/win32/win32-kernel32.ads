pragma Ada_2005;

package Win32.Kernel32 is

   function GetModuleHandle
     (lpModuleName : LPCTSTR_Type)
      return HInstance_Type;
   pragma Import(StdCall,GetModuleHandle,"GetModuleHandleA");

end Win32.Kernel32;
