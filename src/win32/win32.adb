pragma Ada_2005;

with Ada.Unchecked_Conversion;

package body Win32 is

   function MAKEINTRESOURCE
     (wInteger : WORD_Type)
      return LPCTSTR_Type is

      Value : LPCTSTR_Type
        :=Interfaces.C.Strings.Null_Ptr;

      pragma Warnings(Off);
      function Convert is new Ada.Unchecked_Conversion
        (Source => Word_Type,
         Target => LPCTSTR_Type);
      pragma Warnings(On);

   begin
      Value := Convert(wInteger);
      return Value;
   end MAKEINTRESOURCE;

end Win32;
