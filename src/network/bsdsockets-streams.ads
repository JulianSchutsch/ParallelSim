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
--   3.Feb 2012 Julian Schutsch
--     - Original version

with Network.Streams;
with Network.Peers;
with CustomMaps;
with Ada.Unchecked_Deallocation;

package BSDSockets.Streams is
   type Client is new Network.Streams.Channel with private;

   overriding
   procedure Flush
     (Stream: in out Client);

   overriding
   procedure Initialize
     (Item   : not null access Client;
      Config : CustomMaps.StringStringMap.Map);

   overriding
   procedure Finalize
     (Item: not null access Client);

   type Server is new Network.Streams.Server with private;

   overriding
   procedure Initialize
     (Item : not null access Server;
      Config : CustomMaps.StringStringMap.Map);

   overriding
   procedure Finalize
     (Item : not null access Server);

private

   type ClientModeEnum is
     (ClientModeConnecting,
      ClientModeConnected);

   type Server is new Network.Streams.Server with
      record
         SelectEntry : aliased BSDSockets.SelectEntry;
         NextServer  : access Server;
         LastServer  : access Server;
      end record;

   type Client is new Network.Streams.Channel with
      record
         SelectEntry   : aliased BSDSockets.SelectEntry;
         FirstAddrInfo : AddrInfoAccess;
         CurrAddrInfo  : AddrInfoAccess;
         ClientMode    : CLientModeEnum:=ClientModeConnecting;
         Port          : PortID;
         NextClient    : access Client;
         LastClient    : access Client;
      end record;

end BSDSockets.Streams;
