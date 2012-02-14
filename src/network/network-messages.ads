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

pragma Ada_2012;

with Ada.Streams;
with CustomMaps;

package Network.Messages is
   use Ada.Streams;

   StreamOverflow   : Exception;
   InvalidOperation : Exception;
   IncompleteData   : Exception;
   InvalidData      : Exception;

   type ChannelCallBack;
   type ChannelCallBackClassAccess is access ChannelCallBack;

   type Channel(Max: Stream_Element_Count) is
     abstract new Root_Stream_Type with
      record
         ReceivedContent : aliased Stream_Element_Array(0..Max);
         ReceivePosition : Stream_Element_Offset;
         AmountReceived  : Stream_Element_Count;
         WrittenContent  : aliased Stream_Element_Array(0..Max);
         WritePosition   : Stream_Element_Offset;
         CallBack        : ChannelCallBackClassAccess;
      end record;

   type ChannelClassAccess is access Channel'Class;

   type ChannelCallBack is tagged
      record
         Channel : ChannelClassAccess;
      end record;

   procedure OnReceive
     (Item : in out ChannelCallBack) is null;

   overriding
   procedure Read
     (Stream : in out Channel;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);

   overriding
   procedure Write
     (Stream : in out Channel;
      Item   : in Stream_Element_Array);
   ---------------------------------------------------------------------------

end;
