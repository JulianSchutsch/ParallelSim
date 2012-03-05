--   Copyright 2012 Julian Schutsch
--
--   This file is part of ParallelSim
--
--   ParallelSim is free software: you can redistribute it and/or modify
--   it under the terms of the GNU Affero General Public License as published by
--   the Free Software Foundation, either version 3 of the License, or
--   (at your option) any later version.
--
--   ParallelSim is distributed in the hope that it will be useful,
--   but WITHOUT ANY WARRANTY; without even the implied warranty of
--   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--   GNU Affero General Public License for more details.
--
--   You should have received a copy of the GNU Affero General Public License
--   along with ParallelSim.  If not, see <http://www.gnu.org/licenses/>.

pragma Ada_2005;

with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Text_IO; use Ada.Text_IO;

package body Basics is
   use type Ada.Containers.Hash_Type;

   function StringSumHash(id: Unbounded_String) return Ada.Containers.Hash_Type is
      Sum : Ada.Containers.Hash_Type:=0;
   begin
      for i in 1..Length(id) loop
         Sum := Sum+Character'Pos(Element(id,i));
      end loop;
      return Sum;
   end;
   ---------------------------------------------------------------------------

   procedure Put
     (Item : StringStringMap.Map) is

      use type StringStringMap.Cursor;

      Cursor : StringStringMap.Cursor;

   begin
      Cursor:=Item.First;
      while Cursor/=StringStringMap.No_Element loop
         Put(StringStringMap.Key(Cursor));
         Put(" : ");
         Put(StringStringMap.Element(Cursor));
         New_Line;
         Cursor:=StringStringMap.Next(Cursor);
      end loop;
   end;
   ---------------------------------------------------------------------------

   function ConcatElements
     (Item : StringStringMap.Map)
      return Unbounded_String is

      use type StringStringMap.Cursor;

      Cursor    : StringStringMap.Cursor;
      NewString : Unbounded_String;

   begin
      Cursor:=Item.First;
      while Cursor/=StringStringMap.No_Element loop
         NewString:=NewString & StringStringMap.Element(Cursor);
         Cursor:=StringStringMap.Next(Cursor);
      end loop;
      return NewString;
   end ConcatElements;
   ---------------------------------------------------------------------------

end Basics;
