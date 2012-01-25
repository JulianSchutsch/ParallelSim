pragma Ada_2005;

with Types; use Types;

with Interfaces;

package Endianess is

   type LittleEndianInteger16 is new Interfaces.Integer_16;
   type LittleEndianInteger32 is new Interfaces.Integer_32;
   type LittleEndianInteger64 is new Interfaces.Integer_64;
   type LittleEndianCardinal16 is new Interfaces.Unsigned_16;
   type LittleEndianCardinal32 is new Interfaces.Unsigned_32;
   type LittleEndianCardinal64 is new Interfaces.Unsigned_64;

   type BigEndianInteger16 is new Interfaces.Integer_16;
   type BigEndianInteger32 is new Interfaces.Integer_32;
   type BigEndianInteger64 is new Interfaces.Integer_64;
   type BigEndianCardinal16 is new Interfaces.Unsigned_16;
   type BigEndianCardinal32 is new Interfaces.Unsigned_32;
   type BigEndianCardinal64 is new Interfaces.Unsigned_64;

   function ToLittleEndian(Integer: Integer16) return LittleEndianInteger16;
   function ToLittleEndian(Integer: Integer32) return LittleEndianInteger32;
   function ToLittleEndian(Integer: Integer64) return LittleEndianInteger64;

   function FromLittleEndian(Integer: LittleEndianInteger16) return Integer16;
   function FromLittleEndian(Integer: LittleEndianInteger32) return Integer32;
   function FromLittleEndian(Integer: LittleEndianInteger64) return Integer64;

   function ToBigEndian(Integer: Integer16) return BigEndianInteger16;
   function ToBigEndian(Integer: Integer32) return BigEndianInteger32;
   function ToBigEndian(Integer: Integer64) return BigEndianInteger64;

   function FromBigEndian(Integer: BigEndianInteger16) return Integer16;
   function FromBigEndian(Integer: BigEndianInteger32) return Integer32;
   function FromBigEndian(Integer: BigEndianInteger64) return Integer64;

end Endianess;
