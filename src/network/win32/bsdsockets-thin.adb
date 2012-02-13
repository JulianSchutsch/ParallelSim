-------------------------------------------------------------------------------
--   Copyright 2012 Julian Schutsch
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
with Interfaces;
with Interfaces.C;
with Interfaces.C.Strings;
with Interfaces.C.Pointers;
with System;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body BSDSockets.Thin is
   use type Interfaces.C.int;
   use type Interfaces.C.unsigned;

   type WSAData is
      record
         wVersion       : Interfaces.Unsigned_16;
         wHighVersion   : Interfaces.Unsigned_16;
         szDescription  : Interfaces.C.char_array(0..256);
         szSystemStatus : Interfaces.C.char_array(0..128);
         iMaxSockets    : Interfaces.Integer_16;
         iMaxUdpDg      : Interfaces.Integer_16;
         lpVendorInfo   : Interfaces.C.Strings.chars_ptr;
      end record;
   pragma Convention(C,WSADATA);
   ---------------------------------------------------------------------------

   function WSAStartup
     (Version : Interfaces.C.unsigned_short;
      Data    : access WSAData)
      return Interfaces.C.int;
   pragma Import(StdCall,WSAStartup,"WSAStartup");
   ---------------------------------------------------------------------------

   function WSACleanup
     return Interfaces.C.int;
   pragma Import(StdCall,WSACleanup,"WSACleanup");
   ---------------------------------------------------------------------------

   function WSAGetLastError
     return Interfaces.C.int;
   pragma Import(StdCall,WSAGetLastError,"WSAGetLastError");
   ---------------------------------------------------------------------------

   function WSAStringToAddressA
     (AddressString   : Interfaces.C.Strings.chars_ptr;
      AddressFamily   : Interfaces.C.int;
      lpProtocolInfo  : System.Address;
      lpAddress       : access SockAddr_In6;
      lpAddressLength : access Interfaces.c.int)
      return Interfaces.C.int;
   pragma Import(StdCall,WSAStringToAddressA,"WSAStringToAddressA");
   ---------------------------------------------------------------------------

   function INET_PTON
     (AddressFamily : Interfaces.C.int;
      AddrString    : Interfaces.C.Strings.chars_ptr;
      Buffer        : access In_Addr6)
      return Interfaces.C.int is

      Addr   : aliased SockAddr_In6;
      Result : Interfaces.C.int;
      Len    : aliased Interfaces.C.int;

   begin
      Len := SockAddr_In6'Size*8;
      Result:=WSAStringToAddressA(AddressString   => AddrString,
                                  AddressFamily   => AddressFamily,
                                  lpProtocolInfo  => System.Null_Address,
                                  lpAddress       => Addr'Access,
                                  lpAddressLength => Len'Access);
      Buffer.all := Addr.sin6_addr;
      if Result=0 then
         return 1;
      else
         return 0;
      end if;
   end INET_PTON;
   ---------------------------------------------------------------------------

   function Error
     return Interfaces.C.int is
   begin
      return WSAGetLastError;
   end Error;
   ---------------------------------------------------------------------------

   procedure FD_SET
     (Socket : SocketID;
      Set    : access fd_set_struct) is
   begin
      Set.fd_array(Integer(Set.fd_count)) := Socket;
      Set.fd_count := Set.fd_count+1;
   end FD_SET;
   ---------------------------------------------------------------------------

   procedure FD_CLR
     (Socket : SocketID;
      Set    : access fd_set_struct) is
   begin
      for i in Set.fd_array'Range loop
         if Set.fd_array(i)=Socket then
            for j in i..Set.fd_array'Last-1 loop
               Set.fd_array(j):=Set.fd_array(j+1);
            end loop;
            exit;
         end if;
      end loop;
   end FD_CLR;
   ---------------------------------------------------------------------------

   function FD_ISSET
     (Socket : SocketID;
      Set    : access fd_set_struct)
      return Interfaces.C.int is
   begin
      for i in 0..Integer(Set.fd_count)-1 loop
         if Set.fd_array(i)=Socket then
            return 1;
         end if;
      end loop;
      return 0;
   end FD_ISSET;

   procedure FD_ZERO
     (Set : access fd_set_struct) is
   begin
      Set.fd_count:=0;
   end FD_ZERO;
   ---------------------------------------------------------------------------

   -- Stores information obtained during initialization
   WSAInfo : aliased WSAData;

   procedure Initialize is
   begin
      if WSAStartup(16#0202#,WSAInfo'Access)/=0 then
         raise FailedNetworkAPIInitialization;
      end if;
   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize is
   begin
      if WSACleanup/=0 then
         Put("Why is WSACleanup failing?");
         New_Line;
      end if;
   end Finalize;
   ---------------------------------------------------------------------------

end BSDSockets.Thin;
