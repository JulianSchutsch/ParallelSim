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

package body GUI.Combobox is

   procedure AddEntry
     (Item   : access Combobox_Type;
      String : Unbounded_String;
      Color  : Canvas.Color_Type) is
   begin
      Item.Choices.Append
        ((String => String,
          Color  => Color));
   end AddEntry;
   ---------------------------------------------------------------------------

   procedure SetIndex
     (Item  : access Combobox_Type;
      Index : Integer) is
   begin
      if Index not in -1..Integer(Item.Choices.Length) then
         raise IndexOutOfRange;
      end if;
      Item.Index:=Index;
   end SetIndex;
   ---------------------------------------------------------------------------

   function GetIndex
     (Item : access Combobox_Type)
      return Integer is
   begin
      return Item.Index;
   end GetIndex;
   ---------------------------------------------------------------------------

   function GetSelectedEntryString
     (Item : access Combobox_Type)
      return Unbounded_String is

      Cursor : StringAndColorList_Pack.Cursor;

   begin

      if Item.Index/=-1 then
         Cursor:=Item.Choices.First;
         for I in 1..Item.Index loop
            Cursor:=StringAndColorList_Pack.Next(Cursor);
         end loop;
         return StringAndColorList_Pack.Element(Cursor).String;
      else
         return U("");
      end if;

   end GetSelectedEntryString;
   ---------------------------------------------------------------------------

   procedure GetSelectedEntry
     (Item   : access Combobox_Type;
      String : out Unbounded_String;
      Color  : out Canvas.Color_Type) is

      Cursor : StringAndColorList_Pack.Cursor;

   begin

      if Item.Index/=-1 then
         Cursor:=Item.Choices.First;
         for I in 1..Item.Index loop
            Cursor:=StringAndColorList_Pack.Next(Cursor);
         end loop;
         String := StringAndColorList_Pack.Element(Cursor).String;
         Color  := StringAndColorList_Pack.Element(Cursor).Color;
      else
         String := U("");
         Color  := 0;
      end if;

   end GetSelectedEntry;
   ---------------------------------------------------------------------------

   procedure Finalize
     (Item : access Combobox_Type) is
   begin
      GUI.Object_Access(Item).Finalize;
   end Finalize;
   ---------------------------------------------------------------------------

   procedure Initialize
     (Item   : access Combobox_Type;
      Parent : Object_ClassAccess) is
   begin
      GUI.Object_Access(Item).Initialize(Parent);
      Item.Index:=-1;
   end Initialize;
   ---------------------------------------------------------------------------

end GUI.Combobox;
