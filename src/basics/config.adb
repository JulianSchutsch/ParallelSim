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

with Ada.Strings.Unbounded;use Ada.Strings.Unbounded;

package body Config is

   procedure Debug
     (Item : in out Config_Type) is

      use type StringStringMap.Cursor;

      Cursor    : StringStringMap.Cursor;

   begin
      Put("Configuration(Debug):");
      New_Line;

      Cursor:=Item.First;
      while Cursor/=StringStringMap.No_Element loop

         Put("  ");
         Put(StringStringMap.Key(Cursor));
         Put(" : ");
         Put(StringStringMap.Element(Cursor));
         New_Line;

         Cursor:=StringStringMap.Next(Cursor);

      end loop;

   end Debug;
   ---------------------------------------------------------------------------

   procedure SaveToFile
     (Item     : in out Config_Type;
      FileName : String) is

      use type StringStringMap.Cursor;

      File   : Ada.Text_IO.File_Type;
      Cursor : StringStringMap.Cursor;
      Key    : Unbounded_String;
      Value  : Unbounded_String;

   begin
      Ada.Text_IO.Create
        (File => File,
         Name => FileName);

      Cursor:=Item.First;

      while Cursor/=StringStringMap.No_Element loop

         Key   := StringStringMap.Key(Cursor);
         Value := StringStringMap.Element(Cursor);

         if Key="" then
            raise InvalidName;
         end if;

         Ada.Text_IO.Unbounded_IO.Put_Line
           (File => File,
            Item => Key);

         Ada.Text_IO.Unbounded_IO.Put_Line
           (File => File,
            Item => Value);

         Cursor:=StringStringMap.Next(Cursor);
      end loop;
      -- Indicate end of map
      Ada.Text_IO.Put_Line
        (File => File,
         Item => "");

      Ada.Text_IO.Close
        (File => File);

   end SaveToFile;
   ---------------------------------------------------------------------------

   procedure LoadFromFile
     (Item     : in out Config_Type;
      FileName : String) is

      File       : Ada.Text_IO.File_Type;
      Key        : Unbounded_String;
      Value      : Unbounded_String;

   begin
      Ada.Text_IO.Open
        (File => File,
         Name => FileName,
         Mode => Ada.Text_IO.In_File);

      loop

         Ada.Text_IO.Unbounded_IO.Get_Line
           (File => File,
            Item => Key);
         exit when Key="";
         Ada.Text_IO.Unbounded_IO.Get_Line
           (File => File,
            Item => Value);

         Item.Insert
           (Key      => Key,
            New_Item => Value);

      end loop;

      Ada.Text_IO.Close(File);

   end LoadFromFile;
   ---------------------------------------------------------------------------

end Config;
