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

with Network.Packets;
with CustomMaps;

package BSDSockets.Packets is

   type UDPServer is new Network.Packets.PacketServer with
      record
         SelectEntry : aliased BSDSockets.SelectEntry;
      end record;


   overriding
   procedure Initialize(Server : in out UDPServer;
                        Config : CustomMaps.StringStringMap.Map);

   overriding
   procedure Finalize(Server: in out UDPServer);

   overriding
   procedure Send(Server: in out UDPServer;
                  Packet: in out Network.Packets.OutPacket'Class);

   overriding
   procedure Receive(Server: in out UDPServer;
                     Packet: in out Network.Packets.InPacket'Class);

   type UDPClient is new Network.Packets.PacketClient with
      record
         SelectEntry : aliased BSDSockets.SelectEntry;
      end record;

end BSDSockets.Packets;
