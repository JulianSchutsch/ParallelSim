pragma Ada_2012;

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

   function FromLittleEndian(Integer: LittleEndianInteger16) return Integer16 is
   begin
      return Integer16(Integer);
   end;

   function FromLittleEndian(Integer: LittleEndianInteger32) return Integer32 is
   begin
      return Integer32(Integer);
   end;

   function FromLittleEndian(Integer: LittleEndianInteger64) return Integer64 is
   begin
      return Integer64(Integer);
   end;

   function ToBigEndian(Integer: Integer16) return BigEndianInteger16 is
      result : Unsigned_16;
   begin
      result:=Shift_Left(Unsigned_16(Integer) and 16#FF#,8)+
        Shift_Right(Unsigned_16(Integer) and 16#FF00#,8);
      return BigEndianInteger16(result);
   end;

   function ToBigEndian(Integer: Integer32) return BigEndianInteger32 is
      result : Unsigned_32;
   begin
      result:=Shift_Left(Unsigned_32(Integer) and 16#FF#,24)+
        Shift_Left(Unsigned_32(Integer) and 16#FF00#,8)+
        Shift_Right(Unsigned_32(Integer) and 16#FF0000#,8)+
        Shift_Right(Unsigned_32(Integer) and 16#FF000000#,24);
      return BigEndianInteger32(result);
   end;

   function ToBigEndian(Integer: Integer64) return BigEndianInteger64 is
      result : Unsigned_64;
   begin
      result:=Shift_Left(Unsigned_64(Integer) and 16#FF#,56)+
        Shift_Left(Unsigned_64(Integer) and 16#FF00#,40)+
        Shift_Left(Unsigned_64(Integer) and 16#FF0000#,24)+
        Shift_Left(Unsigned_64(Integer) and 16#FF000000#,8)+
        Shift_Right(Unsigned_64(Integer) and 16#FF_00000000#,8)+
        Shift_Right(Unsigned_64(Integer) and 16#FF00_00000000#,24)+
        Shift_Right(Unsigned_64(Integer) and 16#FF0000_00000000#,40)+
        Shift_Right(Unsigned_64(Integer) and 16#FF000000_00000000#,56);
      return BigEndianInteger64(result);
   end;

   function FromBigEndian(Integer: BigEndianInteger16) return Integer16 is
      result : Unsigned_16;
   begin
      result:=Shift_Left(Unsigned_16(Integer) and 16#FF#,8)+
        Shift_Right(Unsigned_16(Integer) and 16#FF00#,8);
      return Integer16(result);
   end;

   function FromBigEndian(Integer: BigEndianInteger32) return Integer32 is
      result : Unsigned_32;
   begin
      result:=Shift_Left(Unsigned_32(Integer) and 16#FF#,24)+
        Shift_Left(Unsigned_32(Integer) and 16#FF00#,8)+
        Shift_Right(Unsigned_32(Integer) and 16#FF0000#,8)+
        Shift_Right(Unsigned_32(Integer) and 16#FF000000#,24);
      return Integer32(result);
   end;

   function FromBigEndian(Integer: BigEndianInteger64) return Integer64 is
      result : Unsigned_64;
   begin
      result:=Shift_Left(Unsigned_64(Integer) and 16#FF#,56)+
        Shift_Left(Unsigned_64(Integer) and 16#FF00#,40)+
        Shift_Left(Unsigned_64(Integer) and 16#FF0000#,24)+
        Shift_Left(Unsigned_64(Integer) and 16#FF000000#,8)+
        Shift_Right(Unsigned_64(Integer) and 16#FF_00000000#,8)+
        Shift_Right(Unsigned_64(Integer) and 16#FF00_00000000#,24)+
        Shift_Right(Unsigned_64(Integer) and 16#FF0000_00000000#,40)+
        Shift_Right(Unsigned_64(Integer) and 16#FF000000_00000000#,56);
      return Integer64(result);
   end;

end Endianess;
