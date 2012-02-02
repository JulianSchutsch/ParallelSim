--   Copyright 2012 Julian Schutsch
--
--   This file is part of ParallelSim
--
--   ParallelSim is free software: you can redistribute it and/or modify
--   it under the terms of the GNU Affero General Public License as published by
--   the Free Software Foundation, either version 3 of the License, or
--   (at your option) any later version.
--
--   ParallelSim is distributed in the hope that it will be useful,
--   but WITHOUT ANY WARRANTY; without even the implied warranty of
--   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--   GNU Affero General Public License for more details.
--
--   You should have received a copy of the GNU Affero General Public License
--   along with ParallelSim.  If not, see <http://www.gnu.org/licenses/>.

-- Revision History
--   2.Feb 2012 Julian Schutsch
--     - Original version

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

package body BSDSockets.Packets is

   procedure Initialize(Server : in out UDPServer;
                        Config : CustomMaps.StringStringMap.Map) is
      PortStr   : Unbounded_String;
      FamilyStr : Unbounded_String;
      Host      : Unbounded_String;
      Port      : PortID;
      Family    : AddressFamilyEnum;
   begin
      PortStr   := Config.Element(To_Unbounded_String("Port"));
      FamilyStr := Config.Element(To_Unbounded_String("Family"));
      Host      := Config.Element(To_Unbounded_String("Host"));
      Port      := PortID'Value(To_String(PortStr));
      if FamilyStr="IPv4" then
         Family:=AF_INET;
      else
         if FamilyStr="IPv6" then
            Family:=AF_INET6;
         else
            raise Network.Packets.InvalidData;
         end if;
      end if;

      Server.SelectEntry.Socket := Socket
        (AddressFamily => Family,
         SocketType    => SOCK_DGRAM,
         Protocol      => IPPROTO_ANY);

      Put("Socket(Initialize):");
      Put(ToString(Server.SelectEntry.Socket));
      New_Line;

      Bind(Socket => Server.SelectEntry.Socket,
           Port   => Port,
           Family => Family,
           Host   => To_String(Host));

   end Initialize;

   procedure Finalize(Server : in out UDPServer) is
   begin
      null;
   end Finalize;

   procedure Send(Server: in out UDPServer;
                  Packet: in out Network.Packets.OutPacket'Class) is
   begin
      null;
   end Send;

   procedure Receive(Server: in out UDPServer;
                     Packet: in out Network.Packets.InPacket'Class) is
   begin
      null;
   end Receive;

end BSDSockets.Packets;
