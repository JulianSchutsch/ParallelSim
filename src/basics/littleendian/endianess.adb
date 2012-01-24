with Interfaces; use Interfaces;

package body Endianess is

   function ToLittleEndian(Integer: Integer16) return LittleEndianInteger16 is
   begin
      return LittleEndianInteger16(Integer);
   end;

   function ToLittleEndian(Integer: Integer32) return LittleEndianInteger32 is
   begin
      return LittleEndianInteger32(Integer);
   end;

   function ToLittleEndian(Integer: Integer64) return LittleEndianInteger64 is
   begin
      return LittleEndianInteger64(Integer);
   end;

   function ToBigEndian(Integer: Integer16) return BigEndianInteger16 is
   begin
      return BigEndianInteger16
        (Shift_Left(Unsigned_16(Integer) and 16#FF#),8)+
         Shift_Right(Unsigned_16(Integer) and 16#FF00#),8);
   end;

end Endianess;
