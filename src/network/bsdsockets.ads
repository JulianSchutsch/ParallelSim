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

-- Revision History
--   27.Jan 2012 Julian Schutsch
--     - Original version

-- Reasons for implementation
--   At the date of the writting of this file, no standard Socket library
--   supporting Ipv6 is implemented. This socket implementation tries to
--   limit itself to a modern implementation of the BSDSockets including
--   GetAddrInfo and Inet_Pton which both are capable of handling IPv6
--   addresses.
--
--   The BSDSockets packet is a wrapper of the plattform specific
--   BSDSockets-Thin packet.
--   Functions do not return arbitrary integers but raise exceptions.

-- Usage
--   The usage of this package is close to the usage of the BSDSocket API
--   directly, but some functions are extended to be easier to handle
--   in Ada.
--   For portability one has to use Initialize and Finalize before using
--   and after using this package. (Example : Windows)

pragma Ada_2012;

with Interfaces.C;
with Interfaces.C.Strings;
with Ada.Unchecked_Conversion;
with Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package BSDSockets is

   FailedNetworkAPIInitialization : Exception;
   FailedSocket           : Exception;
   FailedBind             : Exception;
   FailedListen           : Exception;
   FailedGetAddrInfo      : Exception;
   FailedConnect          : Exception;
   FailedAccept           : Exception;
   EntryAddedToTwoLists   : Exception;
   EntryNotAddedToAnyList : Exception;
   FailedShutdown         : Exception;
   FailedCloseSocket      : Exception;
   FailedSend             : Exception;
   FailedReceive          : Exception;
   FailedRecv             : Exception;

   type SocketID is private;

   type PortID is range 0..65535;

   type AddrInfo is private;
   type In_Addr6 is private;
   type SockAddr_In6 is private;
   type SockAddr is private;
   type SockAddrAccess is access SockAddr;

   type PrivSelectEntry is private;

   type SelectEntry is
      record
         Socket    : SocketID;
         Readable  : Boolean;
         Writeable : Boolean;
         Priv      : PrivSelectEntry;
      end record;

   type SelectList is
      record
         FirstEntry: access SelectEntry;
      end record;

   -- Has Representation --
   type AddressFamilyEnum is
     (AF_INET,
      AF_INET6);

   -- Has Representation --
   type SocketTypeEnum is
     (SOCK_STREAM,
      SOCK_DGRAM,
      SOCK_RAW,
      SOCK_RDM,
      SOCK_SEQPACKET);

   -- Has Representation --
   type ProtocolEnum is
     (IPPROTO_ANY,
      IPPROTO_ICMP,
      IPPROTO_IGMP,
      BTHPROTO_RFCOMM,
      IPPROTO_TCP,
      IPPROTO_UDP,
      IPPROTO_ICMPV6,
      IPPROTO_RM);

   type ShutdownMethodEnum is
     (SD_RECEIVE,
      SD_SEND,
      SD_BOTH);

   type SendEnum is
     (MSG_NONE,
      MSG_OOB,
      MSG_DONTROUTE);

   type AddrInfoAccess is access AddrInfo;
   type AddrInfoAccessAccess is access AddrInfoAccess;

   -- Extended select function
   -- This function accepts a list of SocketSelectEntry each containing
   --  a socket and a read- and writeable flag.
   procedure SSelect
     (Sockets : in out SelectList);

   procedure AddEntry
     (List: access SelectList;
      Entr: access SelectEntry);

   procedure RemoveEntry
     (Entr: access SelectEntry);

   procedure Bind
     (Socket : SocketID;
      Port   : PortID;
      Family : AddressFamilyEnum;
      Host   : String := "");

   procedure Listen
     (Socket  : SocketID;
      Backlog : Integer:=0);

   function Socket
     (AddressFamily : AddressFamilyEnum;
      SocketType    : SocketTypeEnum;
      Protocol      : ProtocolEnum)
      return SocketID;

   function Socket
     (AddrInfo : AddrInfoAccess)
      return SocketID;

   procedure Connect
     (Socket   : SocketID;
      AddrInfo : not null AddrInfoAccess;
      Port     : PortID);

   function AAccept
     (Socket : SocketID;
      Host   : out Unbounded_String;
      Port   : out PortID) return SocketID;

   procedure CloseSocket
     (Socket : SocketID);

   procedure ShutDown
     (Socket : SocketID;
      Method : ShutDownMethodEnum);

   function GetAddrInfo
     (AddressFamily : AddressFamilyEnum;
      SocketType    : SocketTypeEnum;
      Protocol      : ProtocolEnum;
      Host          : String)
      return AddrInfoAccess;

   function Send
     (Socket : SocketID;
      Data   : Ada.Streams.Stream_Element_Array;
      Flags  : SendEnum)
      return Ada.Streams.Stream_Element_Count;

   function Recv
     (Socket : SocketID;
      Data   : in out Ada.Streams.Stream_Element_Array;
      Flags  : SendEnum)
      return Ada.Streams.Stream_Element_Count;

   procedure FreeAddrInfo
     (AddrInfo: not null AddrInfoAccess);

   function AddrInfo_Next
     (AddrInfo: AddrInfoAccess)
      return AddrInfoAccess;

   function ToString
     (Port : PortID)
      return String;

   function ToString
     (Socket : SocketID)
      return String;

   -- Default Select List, is processed by Process procedure (below)
   DefaultSelectList : aliased SelectList;

   procedure Process;
   procedure Initialize;
   procedure Finalize;

private

   type PrivSelectEntry is
      record
         Next : access SelectEntry := null;
         Last : access SelectEntry := null;
         List : access SelectList  := null;
      end record;

   type SockAddr is
      record
         sa_family : Interfaces.C.short;
         -- sa_port is the only field which is required using
         --  connect in combination with getaddrinfo
         sa_port   : Interfaces.C.unsigned_short;
         sa_data   : Interfaces.C.char_array(0..11);
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
   pragma Convention(C,In_Addr6);

   type SockAddr_In6 is
      record
         sin6_family   : Interfaces.C.short;
         sin6_port     : Interfaces.C.unsigned_short;
         sin6_flowinfo : Interfaces.C.unsigned_long;
         sin6_addr     : aliased In_Addr6;
         sin6_scope_id : Interfaces.C.unsigned_long;
      end record;
   pragma Convention(C,Sockaddr_In6);

   type AddrInfo is
      record
         ai_flags     : Interfaces.C.int;
         ai_family    : Interfaces.C.int;
         ai_socktype  : Interfaces.C.int;
         ai_protocol  : Interfaces.C.int;
         ai_addrlen   : Interfaces.C.size_t;
         ai_addr      : SockAddrAccess;
         ai_canonname : Interfaces.C.Strings.chars_ptr;
         ai_next      : AddrInfoAccess;
      end record;
   pragma Convention(C,AddrInfo);

   type SocketID is new Interfaces.C.int;

   -- Representation --

   -- Representation --
   for AddressFamilyEnum use
     (AF_INET      => 2,
      AF_INET6     => 23);
   for AddressFamilyEnum'Size use Interfaces.C.int'Size;

   -- Representation --
   for SocketTypeEnum use
     (SOCK_STREAM    => 1,
      SOCK_DGRAM     => 2,
      SOCK_RAW       => 3,
      SOCK_RDM       => 4,
      SOCK_SEQPACKET => 5);
   for SocketTypeEnum'Size use Interfaces.C.int'Size;

   -- Representation --
   for ProtocolEnum use
     (IPPROTO_ANY     => 0,
      IPPROTO_ICMP    => 1,
      IPPROTO_IGMP    => 2,
      BTHPROTO_RFCOMM => 3,
      IPPROTO_TCP     => 6,
      IPPROTO_UDP     => 17,
      IPPROTO_ICMPV6  => 58,
      IPPROTO_RM      => 113);
   for ProtocolEnum'Size use Interfaces.C.int'Size;

   -- Representation --
   for ShutdownMethodEnum use
     (SD_RECEIVE => 0,
      SD_SEND    => 1,
      SD_BOTH    => 2);
   for ShutDownMethodEnum'Size use Interfaces.C.int'Size;

   -- Representation --
   for SendEnum use
     (MSG_NONE      => 0,
      MSG_OOB       => 1,
      MSG_DONTROUTE => 4);

   for SendEnum'Size use Interfaces.C.int'Size;

end BSDSockets;
