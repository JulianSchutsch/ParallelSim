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
with Ada.Streams;

package BSDSockets.Thin is

   FD_SETSIZE: constant Natural:=1024;

   type fd_set_array is array(0..FD_SETSIZE-1) of SocketID;
   pragma Convention(C,fd_set_array);

   type fd_set_struct is
      record
         fd_count : Interfaces.C.unsigned;
         fd_array : fd_set_array;
      end record;
   pragma Convention(C,fd_set_struct);

   type TimeVal is
      record
         tv_sec  : Interfaces.C.long;
         tv_usec : Interfaces.C.long;
      end record;
   pragma Convention(C,TimeVal);

   function Error return Interfaces.C.int;

   function Socket(AddressFamily : Interfaces.C.int;
                   SocketType    : Interfaces.C.int;
                   ProtocolType  : Interfaces.C.int) return Interfaces.C.int;
   pragma Import(StdCall,Socket,"socket");

   function Bind(Socket  : Interfaces.C.int;
                 Name    : access SockAddr_In6;
                 NameLen : Interfaces.C.int) return Interfaces.C.int;
   pragma Import(StdCall,Bind,"bind");

   function Listen(Socket  : Interfaces.C.int;
                   Backlog : Interfaces.C.int) return Interfaces.C.int;
   pragma Import(StdCall,Listen,"listen");

   function Connect(Socket  : Interfaces.C.int;
                    Name    : SockAddrAccess;
                    NameLen : Interfaces.C.int) return Interfaces.C.int;
   pragma Import(StdCall,Connect,"connect");

   function GetAddrInfo(pNodeName    : Interfaces.C.Strings.chars_ptr;
                        pServiceName : Interfaces.C.Strings.chars_ptr;
                        pHints       : access AddrInfo;
                        ppResult     : access AddrInfoAccess) return Interfaces.C.int;
   pragma Import(StdCall,GetAddrInfo,"getaddrinfo");

   procedure FreeAddrInfo(pAddrInfo: access AddrInfo);
   pragma Import(StdCall,FreeAddrInfo,"freeaddrinfo");

   function INET_PTON(AddressFamily : Interfaces.C.int;
                      AddrString    : Interfaces.C.Strings.chars_ptr;
                      Buffer        : access In_Addr6) return Interfaces.C.int;

   function HTONS(HostShort : Interfaces.C.unsigned_short) return Interfaces.C.unsigned_short;
   pragma Import(StdCall,HTONS,"htons");

   function SSelect(NumberOfSockets: Interfaces.C.int;
                    ReadSet         : access fd_set_struct;
                    WriteSet        : access fd_set_struct;
                    ExceptSet       : access fd_set_struct;
                    TimeOut         : access TimeVal) return Interfaces.C.int;
   pragma Import(StdCall,SSelect,"select");

   function AAccept(Socket  : Interfaces.C.int;
                    Addr    : access SockAddr;
                    AddrLen : access Interfaces.C.int) return Interfaces.C.int;
   pragma Import(StdCall,AAccept,"accept");

   function Recv(Socket : Interfaces.C.int;
                 Buf    : access Ada.Streams.Stream_Element_Array;
                 Len    : Interfaces.C.int;
                 Flags  : Interfaces.C.int) return Interfaces.C.int;
   pragma Import(StdCall,Recv,"recv");

   function Send(Socket : Interfaces.C.int;
                 Buf    : access Ada.Streams.Stream_Element_Array;
                 Len    : Interfaces.C.int;
                 Flags  : Interfaces.C.int) return Interfaces.C.int;
   pragma Import(StdCall,Send,"send");

   procedure FD_SET(Socket : SocketID;
                    Set    : access fd_set_struct);

   procedure FD_CLR(Socket : SocketID;
                    Set    : access fd_set_struct);

   function FD_ISSET(Socket : SocketID;
                      Set    : access fd_set_struct) return Interfaces.C.int;

   procedure FD_ZERO(Set : access fd_set_struct);

   procedure Initialize;
   procedure Finalize;

private


end BSDSockets.Thin;
