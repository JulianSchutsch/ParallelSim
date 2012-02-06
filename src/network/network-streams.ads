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

-- Reasons for implementation
--   This modul provides an abstract tagged type for easy replacement
--   of network stream implementations.
--   It also implements stream read and write acting on input and output
--   contents separately.

with Ada.Streams; use Ada.Streams;
with Ada.Unchecked_Deallocation;
with CustomMaps;
with Network.Peers;

package Network.Streams is

   StreamOverflow : Exception;
   IncompleteData   : Exception;
   InvalidData      : Exception;

   type Channel(Max: Stream_Element_Count) is abstract new Root_Stream_Type with
      record
         ReceivedContent : Stream_Element_Array(0..Max);
         ReceivePosition : Stream_Element_Offset;
         AmountReceived  : Stream_Element_Count;
         WrittenContent  : Stream_Element_Array(0..Max);
         WritePosition   : Stream_Element_Offset;
         PeerData        : Network.Peers.PeerData;
      end record;

   type ChannelClassAccess is access Channel'Class;

   type Server is abstract tagged
      record
         OnNewChannel: access procedure(Item: Channel);
      end record;

   type ServerClassAccess is access Server'Class;

   overriding
   procedure Read
     (Stream : in out Channel;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);

   overriding
   procedure Write
     (Stream : in out Channel;
      Item   : in Stream_Element_Array);

   procedure Initialize
     (Item   : not null access Channel;
      Config : CustomMaps.StringStringMap.Map) is abstract;

   procedure Finalize
     (Item : not null access Channel) is abstract;

   procedure Flush
     (Item : in out Channel) is abstract;

   procedure Initialize
     (Item   : not null access Server;
      Config : CustomMaps.StringStringMap.Map) is abstract;

   procedure Finalize
     (Item : not null access Server) is abstract;

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Channel'Class,
      Name   => ChannelClassAccess);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Server'Class,
      Name   => ServerClassAccess);

end Network.Streams;
