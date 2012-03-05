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

pragma Ada_2005;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Unchecked_Deallocation;

package body Logging.StdOut is

   type Channel_Access is access all Channel_Type;

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Channel_Type,
      Name   => Channel_Access);

   procedure Write
     (Item    : in out Channel_Type;
      Message : Unbounded_String) is
   begin
      Put("[");
      Put(Item.ModuleName);
      for i in 20..Length(Item.ModuleName) loop
         Put(" ");
      end loop;
      Put("][");
      Put(Item.ChannelName);
      for i in 20..Length(Item.ChannelName) loop
         Put(" ");
      end loop;
      Put("]:");
      Put(Message);
   end Write;
   ---------------------------------------------------------------------------

   function NewChannel
     (ModuleName  : Unbounded_String;
      ChannelName : Unbounded_String)
      return Channel_ClassAccess is

      NewChannel : Channel_Access;

   begin
      NewChannel := new Channel_Type;
      NewChannel.ModuleName  := ModuleName;
      NewChannel.ChannelName := ChannelName;
      return Channel_ClassAccess(NewChannel);
   end;
   ---------------------------------------------------------------------------

   procedure FreeChannel
     (Item : Logging.Channel_ClassAccess) is

      Channel : Channel_Access;

   begin
      Channel := Channel_Access(Item);
      Free(Channel);
   end FreeChannel;
   ---------------------------------------------------------------------------

   Implementation : constant Implementation_Type:=
     (NewChannel  => NewChannel'Access,
      FreeChannel => FreeChannel'Access);

   procedure Register is
   begin
      Implementations.Register
        (Identifier     => To_Unbounded_String("StdOut"),
         Implementation => Implementation);
   end Register;
   ---------------------------------------------------------------------------

end Logging.StdOut;
