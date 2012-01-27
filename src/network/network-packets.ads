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

with Ada.Streams;

package Network.Packets is
   use Ada.Streams;

   PacketOverflow   : Exception;
   InvalidOperation : Exception;

   type InPacket(Max: Stream_Element_Offset) is new Root_Stream_Type with
      record
         Content  : Stream_Element_Array(0..Max);
         Position : Stream_Element_Offset:=0;
      end record;

   type OutPacket(Max: Stream_Element_Offset) is new Root_Stream_Type with
      record
         Content  : Stream_Element_Array(0..Max);
         Position : Stream_Element_Offset:=0;
      end record;

   type InPacketAccess is access InPacket;
   type OutPacketAccess is access OutPacket;

   overriding
   procedure Read(Stream : in out InPacket;
                  Item   : out Stream_Element_Array;
                  Last   : out Stream_Element_Offset);

   overriding
   procedure Write(Stream : in out InPacket;
                   Item   : in Stream_Element_Array);

   overriding
   procedure Read(Stream : in out OutPacket;
                  Item   : out Stream_Element_Array;
                  Last   : out Stream_Element_Offset);

   overriding
   procedure Write(Stream : in out OutPacket;
                   Item   : in Stream_Element_Array);

   procedure Copy(Source : not null access OutPacket'Class;
                  Dest   : not null access InPacket'Class);

   procedure Debug(Packet: in InPacket);
   procedure Debug(Packet: in OutPacket);

private

end;
