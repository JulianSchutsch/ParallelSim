-------------------------------------------------------------------------------
--   Copyright 2011 Julian Schutsch
--
--   This file is part of ParallelSim
--
--   ParallelSim is free software: you can redistribute it and/or modify
--   it under the terms of the GNU Affero General Public License as published
--   by the Free Software Foundation, either version 3 of the License, or
--   (at your option) any later version.
--
--   ParallelSim is distributed in the hope that it will be useful,
--   but WITHOUT ANY WARRANTY; without even the implied warranty of
--   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--   GNU Affero General Public License for more details.
--
--   You should have received a copy of the GNU Affero General Public License
--   along with ParallelSim.  If not, see <http://www.gnu.org/licenses/>.
-------------------------------------------------------------------------------

-- Revision History
--   27.Jan 2012 Julian Schutsch
--     - Original version

with Interfaces;
with Interfaces.C;
with Interfaces.C.Strings;
with System;

package BSDSockets.Thin is

   type SockAddr is
      record
         sa_family : Interfaces.C.unsigned_short;
         sa_data: Interfaces.C.char_array(0..13);
      end record;
   pragma Convention(C,SockAddr);

   type In_Addr is
      record
         S_Addr : Interfaces.C.unsigned_long;
      end record;
   pragma Convention(C,In_Addr);

   type SockAddr_In is
      record
         sin_family : Interfaces.C.short;
         sin_port   : Interfaces.C.unsigned_short;
         sin_addr   : In_Addr;
         sin_zero   : Interfaces.C.char_array(0..7);
      end record;
   pragma Convention(C,SockAddr_In);

   type In_Addr6 is
      record
         s6_addr:Interfaces.C.char_array(0..15);
      end record;

   type SockAddr_In6 is
      record
         sin6_family   : Interfaces.C.short;
         sin6_port     : Interfaces.C.unsigned_short;
         sin6_flowinfo : Interfaces.C.unsigned_long;
         sin6_addr     : In_Addr6;
         sin6_scope_id : Interfaces.C.unsigned_long;
      end record;
   pragma Convention(C,Sockaddr_In6);

   function Error return Interfaces.C.int;

   function Socket(AddressFamily : Interfaces.C.int;
                   SocketType    : Interfaces.C.int;
                   ProtocolType  : Interfaces.C.int) return Interfaces.C.int;
   pragma Import(StdCall,Socket,"socket");

   function Bind(Socket  : Interfaces.C.int;
                 Name    : access SockAddr_In6;
                 NameLen : Interfaces.C.int) return Interfaces.C.int;
   pragma Import(StdCall,Bind,"bind");

   function INET_PTON(AddressFamily : Interfaces.C.int;
                      AddrString    : Interfaces.C.Strings.chars_ptr;
                      Buffer        : System.Address) return Interfaces.C.int;
   pragma Import(StdCall,INET_PTON,"inet_pton");

   procedure Initialize;
   procedure Finalize;

private


end BSDSockets.Thin;
