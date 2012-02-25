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

-- Usage
--   The network stream is available through two tagged types:
--
--    Server  : Accepts incoming connections and creates channels to
--              represent them.
--
--    Channel : Can either be a client side connection or a server side
--              connection. Acts as a stream with input and output separated.
--
--   Both communicate back by use of communication tagged types:
--
--     ServerCallBack
--     ChannelCallBack
--
--   Which are assigned to a callback entry in Server or Channel by the
--   using component (for example when OnAccept is called).

with Ada.Streams; use Ada.Streams;
with Ada.Unchecked_Deallocation;
with Basics; use Basics;

package Network.Streams is

   StreamOverflow : Exception;
   IncompleteData   : Exception;
   InvalidData      : Exception;

   type ProcessAccess is
     access procedure;

   type ChannelCallBack;
   type ChannelCallBackClassAccess is access all ChannelCallBack'Class;

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

   type ChannelClassAccess is access all Channel'Class;
   type ClientClassAccess is access all Channel'Class;

   type ClientConstructor is access function
     (Config : StringStringMap.Map)
      return ClientClassAccess;

   type ClientDestructor is access procedure
     (Item : in out ClientClassAccess);

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

   type ChannelCallBack is tagged limited
      record
         Channel : ChannelClassAccess;
      end record;

   procedure OnReceive
     (Item : in out ChannelCallBack) is null;

   procedure OnDisconnect
     (Item : in out ChannelCallBack) is null;

   procedure OnFailedConnect
     (Item : in out ChannelCallBack) is null;
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------

   type ServerCallBack;
   type ServerCallBackClassAccess is access all ServerCallBack'Class;

   type Server is abstract tagged
      record
         CallBack : ServerCallBackClassAccess;
      end record;
   type ServerClassAccess is access all Server'Class;

   type ServerConstructor is access function
     (Config : StringStringMap.Map)
      return ServerClassAccess;

   type ServerDestructor is access procedure
     (Item : in out ServerClassAccess);
   ---------------------------------------------------------------------------

   type ServerCallBack is tagged null record;

   procedure OnAccept
     (Item : in out ServerCallBack;
      Chan : ChannelClassAccess) is null;
   ---------------------------------------------------------------------------

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Channel'Class,
      Name   => ChannelClassAccess);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Channel'Class,
      Name   => ClientClassAccess);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Server'Class,
      Name   => ServerClassAccess);

end Network.Streams;
