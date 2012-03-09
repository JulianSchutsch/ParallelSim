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

with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;

package body Network.Streams is

   procedure DebugSend
     (Stream : Channel_Type'Class) is
   begin
      Put("Debug Send:");
      Put(" 0..");
      Put(Integer(Stream.WritePosition));
      New_Line;
      New_Line;
      for i in 0..Stream.WritePosition-1 loop
         Put(" ");
         Put(Integer(Stream.WrittenContent(i)));
         New_Line;
      end loop;
   end DebugSend;
   ---------------------------------------------------------------------------

   procedure DebugReceive
     (Stream : Channel_Type'Class) is
   begin
      Put("Debug Receive:");
      Put(" 0..");
      Put(Integer(Stream.AmountReceived)-1);
      New_Line;
      New_Line;
      for i in 0..Stream.AmountReceived-1 loop
         Put(" ");
         Put(Integer(Stream.ReceivedContent(i)));
         New_Line;
      end loop;
   end DebugReceive;
   ---------------------------------------------------------------------------

   procedure Read
     (Stream : in out Channel_Type;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset) is
   begin
      Put("(");
      Last := Stream.ReceivePosition+Item'Last;
      if Last>Stream.AmountReceived then
         Put("]");
         raise StreamOverflow;
      end if;
      Item := Stream.ReceivedContent(Stream.ReceivePosition..Last-1);
      Stream.ReceivePosition := Last;
      Put(")");
   end Read;
   ---------------------------------------------------------------------------

   procedure Write
     (Stream : in out Channel_Type;
      Item   : in Stream_Element_Array) is

      Last : Stream_Element_Offset;

   begin
      Last := Stream.WritePosition+Item'Last;
      if Last>Stream.WrittenContent'Last then
         raise StreamOverflow;
      end if;
      Stream.WrittenContent(Stream.WritePosition..Last-1):=Item;
      Stream.WritePosition:=Last;
   end Write;
   ---------------------------------------------------------------------------

end Network.Streams;
