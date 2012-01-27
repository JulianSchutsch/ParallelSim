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

pragma Ada_2012;

with Interfaces.C;
with Ada.Unchecked_Conversion;

package BSDSockets is

   FailedNetworkAPIInitialization : Exception;
   FailedToCreateSocket           : Exception;

   type SocketID is private;
   type PortID is new Interfaces.C.int;
   type AddressFamilyEnum is (AF_UNSPEC,
                              AF_INET,
                              AF_IPX,
                              AF_APPLETALK,
                              AF_NETBIOS,
                              AF_INET6,
                              AF_IRDA,
                              AF_BTH);

   type SocketTypeEnum is (SOCK_STREAM,
                           SOCK_DGRAM,
                           SOCK_RAW,
                           SOCK_RDM,
                           SOCK_SEQPACKET);

   type ProtocolEnum is (IPPROTO_ANY,
                         IPPROTO_ICMP,
                         IPPROTO_IGMP,
                         BTHPROTO_RFCOMM,
                         IPPROTO_TCP,
                         IPPROTO_UDP,
                         IPPROTO_ICMPV6,
                         IPPROTO_RM);

   for AddressFamilyEnum use
     (AF_UNSPEC=>0,
      AF_INET=>4,
      AF_IPX=>6,
      AF_APPLETALK=>16,
      AF_NETBIOS=>17,
      AF_INET6=>23,
      AF_IRDA=>26,
      AF_BTH=>32);

   for AddressFamilyEnum'Size use Interfaces.C.int'Size;

   for SocketTypeEnum use
     (SOCK_STREAM=>1,
      SOCK_DGRAM=>2,
      SOCK_RAW=>3,
      SOCK_RDM=>4,
      SOCK_SEQPACKET=>5);

   for SocketTypeEnum'Size use Interfaces.C.int'Size;

   for ProtocolEnum use
     (IPPROTO_ANY=>0,
      IPPROTO_ICMP=>1,
      IPPROTO_IGMP=>2,
      BTHPROTO_RFCOMM=>3,
      IPPROTO_TCP=>6,
      IPPROTO_UDP=>17,
      IPPROTO_ICMPV6=>58,
      IPPROTO_RM=>113);

   for ProtocolEnum'Size use Interfaces.C.int'Size;

   function AddressFamilyToInt is new Ada.Unchecked_Conversion(Source => AddressFamilyEnum,
                                                               Target => Interfaces.C.int);
   function SocketTypeToInt is new Ada.Unchecked_Conversion(Source => SocketTypeEnum,
                                                            Target => Interfaces.C.int);
   function ProtocolToInt is new Ada.Unchecked_Conversion(Source => ProtocolEnum,
                                                          Target => Interfaces.C.int);

--   procedure Bind
--     (Socket : SocketID;
--      Port   : PortID;
--      Host   : String := "");

   function Socket
     (AddressFamily : AddressFamilyEnum;
      SocketType    : SocketTypeEnum;
      Protocol      : ProtocolEnum) return SocketID;

   procedure Initialize;

   procedure Finalize;

private

   type SocketID is new Interfaces.C.int;

end BSDSockets;
