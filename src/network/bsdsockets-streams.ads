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

pragma Ada_2005;

with Network.Streams;
with Basics; use Basics;
with Ada.Calendar;

package BSDSockets.Streams is

   type BSDSocketChannel_Type is new Network.Streams.Channel_Type with private;
   type Client_Type is new BSDSocketChannel_Type with private;
   type Client_Access is access all Client_Type;

   function NewStreamClient
     (Config : StringStringMap.Map)
      return Network.Streams.Client_ClassAccess;

   procedure FreeStreamClient
     (Item : in out Network.Streams.Client_ClassAccess);
   ---------------------------------------------------------------------------

   type Server_Type is new Network.Streams.Server_Type with private;
   type Server_Access is access all Server_Type;

   function NewStreamServer
     (Config : StringStringMap.Map)
      return Network.Streams.Server_ClassAccess;

   procedure FreeStreamServer
     (Item : in out Network.Streams.Server_ClassAccess);
   ---------------------------------------------------------------------------

   procedure Register;

private

   type BSDSocketChannel_Type is new Network.Streams.Channel_Type with
      record
         SelectEntry : aliased BSDSockets.SelectEntry;
      end record;

   type ServerChannel_Type;

   type ServerChannel_Access is access all ServerChannel_Type;

   type ServerChannel_Type is new BSDSocketChannel_Type with
      record
         NextChannel : ServerChannel_Access;
         LastChannel : ServerChannel_Access;
         Server      : Server_Access;
      end record;
   ---------------------------------------------------------------------------

   type Server_Type is new Network.Streams.Server_Type with
      record
         SelectEntry  : aliased BSDSockets.SelectEntry;
         NextServer   : Server_Access;
         LastServer   : Server_Access;
         FirstChannel : ServerChannel_Access;
      end record;
   ---------------------------------------------------------------------------

   type ClientModeEnum is
     (ClientModeConnecting,
      ClientModeConnected,
      ClientModeFailedConnect);

   type Client_Type is new BSDSocketChannel_Type with
      record
         FirstAddrInfo : AddrInfoAccess;
         CurrAddrInfo  : AddrInfoAccess;
         ClientMode    : ClientModeEnum:=ClientModeConnecting;
         LastTime      : Ada.Calendar.Time;
         Port          : PortID;
         NextClient    : Client_Access;
         LastClient    : Client_Access;
      end record;
   ---------------------------------------------------------------------------

end BSDSockets.Streams;
