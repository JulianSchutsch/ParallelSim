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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Network.Packets is

   procedure Debug(Packet: in OutPacket) is
   begin
      for i in Packet.Content'Range loop
         Put(Integer(Packet.Content(i)));
      end loop;
      New_Line;
   end Debug;

   procedure Debug(Packet: in InPacket) is
   begin
      for i in Packet.Content'Range loop
         Put(Integer(Packet.Content(i)));
      end loop;
      New_Line;
   end Debug;

   procedure Copy(Source : not null access OutPacket'Class;
                  Dest   : not null access InPacket'Class) is
   begin
      Put("Copy complete packet");
      Put(Integer(Source.Position));
      New_Line;
      Dest.Content(0..Source.Position-1)  := Source.Content(0..Source.Position-1);
      Dest.Position := 0;
      Dest.Filled   := Source.Position;
   end Copy;

   procedure Read(Stream : in out InPacket;
                  Item   : out Stream_Element_Array;
                  Last   : out Stream_Element_Offset) is
   begin
      Put("Read Element");
      Put(Integer(Stream.Position));
      Put(Integer(Stream.Position+Item'Last));
      New_Line;
      Last := Stream.Position+Item'Last;
      if Last>Stream.Filled then
         raise PacketOverflow;
      end if;
      Item := Stream.Content(Stream.Position..Last-1);
      Stream.Position := Last;
   end Read;

   procedure Write(Stream : in out InPacket;
                   Item   : in Stream_Element_Array) is
   begin
      raise InvalidOperation;
   end Write;

   procedure Read(Stream : in out OutPacket;
                  Item   : out Stream_Element_Array;
                  Last   : out Stream_Element_Offset) is
   begin
      raise InvalidOperation;
   end Read;

   procedure Write(Stream : in out OutPacket;
                   Item   : in Stream_Element_Array) is
      Last : Stream_Element_Offset;
   begin
      Put("Write Element");
      Put(Integer(Stream.Position));
      Put(Integer(Stream.Position+Item'Last));
      New_Line;
      Last := Stream.Position+Item'Last;
      if Last>Stream.Content'Last then
         raise PacketOverflow;
      end if;
      Stream.Content(Stream.Position..Last-1):=Item;
      Stream.Position:=Last;
   end Write;

end Network.Packets;
