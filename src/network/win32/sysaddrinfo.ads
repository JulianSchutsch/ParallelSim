with Interfaces.C;
with Interfaces.C.Strings;
with System;

package SysAddrInfo is

   type AddrInfo is
      record
         ai_flags     : Interfaces.C.int;
         ai_family    : Interfaces.C.int;
         ai_socktype  : Interfaces.C.int;
         ai_protocol  : Interfaces.C.int;
         ai_addrlen   : Interfaces.C.size_t;
         ai_canonname : Interfaces.C.Strings.chars_ptr;
         ai_addr      : System.Address;
         ai_next      : System.Address;
      end record;
   pragma Convention(C,AddrInfo);


end SysAddrInfo;
