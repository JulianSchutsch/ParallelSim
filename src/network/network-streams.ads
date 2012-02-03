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
with CustomMaps;
with Network.Peers;

package Network.Streams is

   StreamOverflow : Exception;
   IncompleteData   : Exception;
   InvalidData      : Exception;

   type StreamType(Max: Stream_Element_Count) is abstract new Root_Stream_Type with
      record
         ReceivedContent : Stream_Element_Array(0..Max);
         ReceivePosition : Stream_Element_Offset;
         AmountReceived  : Stream_Element_Count;
         WrittenContent  : Stream_Element_Array(0..Max);
         WritePosition   : Stream_Element_Offset;
         OnNewPeer       : access procedure(Data: out Network.Peers.PeerData);
      end record;

   overriding
   procedure Read
     (Stream : in out StreamType;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);

   overriding
   procedure Write
     (Stream : in out StreamType;
      Item   : in Stream_Element_Array);

   procedure Initialize
     (Stream : in out StreamType;
      Config : CustomMaps.StringStringMap.Map) is abstract;

   procedure Finalize
     (Stream : in out StreamType) is abstract;

   procedure Flush
     (Stream : in out StreamType) is abstract;


end Network.Streams;
