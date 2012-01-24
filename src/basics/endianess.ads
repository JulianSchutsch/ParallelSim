pragma Ada2005;

with Types; use Types;

package Endianess is
   function Integer32ToLittleEndian(Integer : Integer32) return Integer32;

end Endianess;
