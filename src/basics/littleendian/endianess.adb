package body Endianess is
   function Integer32ToLittleEndian(Integer : Integer32) return Integer32 is
   begin
      return Integer;
   end;
end Endianess;
