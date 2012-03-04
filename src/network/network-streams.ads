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
pragma Ada_2005;

with Ada.Streams; use Ada.Streams;
with Ada.Unchecked_Deallocation;
with Basics; use Basics;

package Network.Streams is

   pragma Elaborate_Body;

   StreamOverflow : Exception;
   IncompleteData   : Exception;
   InvalidData      : Exception;

   type ProcessAccess is
     access procedure;

   type Initialize_Access is
     access procedure;

   type Finalize_Access is
     access procedure;

   type ChannelCallBack_Type;
   type ChannelCallBack_ClassAccess is access all ChannelCallBack_Type'Class;

   type Channel_Type(Max: Stream_Element_Count) is
     abstract new Root_Stream_Type with
      record
         ReceivedContent : aliased Stream_Element_Array(0..Max);
         ReceivePosition : Stream_Element_Offset:=0;
         AmountReceived  : Stream_Element_Count:=0;
         WrittenContent  : aliased Stream_Element_Array(0..Max);
         WritePosition   : Stream_Element_Offset:=0;
         CallBack        : ChannelCallBack_ClassAccess:=null;
      end record;

   type Channel_ClassAccess is access all Channel_Type'Class;
   type Client_ClassAccess is access all Channel_Type'Class;

   type Client_Constructor is access function
     (Config : StringStringMap.Map)
      return Client_ClassAccess;

   type Client_Destructor is access procedure
     (Item : in out Client_ClassAccess);

   overriding
   procedure Read
     (Stream : in out Channel_Type;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);

   overriding
   procedure Write
     (Stream : in out Channel_Type;
      Item   : in Stream_Element_Array);
   ---------------------------------------------------------------------------

   type ChannelCallBack_Type is tagged limited
      record
         null;
      end record;

   procedure OnCanSend
     (Item : in out ChannelCallBack_Type) is null;

   procedure OnReceive
     (Item : in out ChannelCallBack_Type) is null;

   procedure OnConnect
     (Item : in out ChannelCallBack_Type) is null;

   procedure OnDisconnect
     (Item : in out ChannelCallBack_Type) is null;

   procedure OnFailedConnect
     (Item  : in out ChannelCallBack_Type;
      Retry : in out Boolean) is null;
   ---------------------------------------------------------------------------

   type ServerCallBack_Type;
   type ServerCallBack_ClassAccess is access all ServerCallBack_Type'Class;

   type Server_Type is abstract tagged
      record
         CallBack : ServerCallBack_ClassAccess:=null;
      end record;
   type Server_ClassAccess is access all Server_Type'Class;

   type Server_Constructor is access function
     (Config : StringStringMap.Map)
      return Server_ClassAccess;

   type Server_Destructor is access procedure
     (Item : in out Server_ClassAccess);
   ---------------------------------------------------------------------------

   type ServerCallBack_Type is tagged null record;

   procedure OnAccept
     (Item    : in out ServerCallBack_Type;
      Channel : Channel_ClassAccess) is null;
   ---------------------------------------------------------------------------

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Channel_Type'Class,
      Name   => Channel_ClassAccess);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Channel_Type'Class,
      Name   => Client_ClassAccess);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Server_Type'Class,
      Name   => Server_ClassAccess);

end Network.Streams;
