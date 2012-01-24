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

   function ToBigEndian(Integer: Integer16) return BigEndianInteger16;

end Endianess;
