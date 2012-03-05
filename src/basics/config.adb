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
with Ada.Strings.Unbounded;use Ada.Strings.Unbounded;

package body Config is
   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Module,
      Name   => ModuleAccess);

   procedure Debug
     (Item : in out Config_Type) is

      use type StringStringMap.Cursor;

      ModuleCursor : ModuleAccess;
      MapCursor    : StringStringMap.Cursor;

   begin
      Put("Configuration(Debug):");
      New_Line;
      ModuleCursor:=Item.First;
      while ModuleCursor/=null loop

         New_Line;
         Put("Module:");
         Put(ModuleCursor.Name);
         New_Line;
         New_Line;

         MapCursor:=ModuleCursor.Map.First;
         while MapCursor/=StringStringMap.No_Element loop

            Put("  ");
            Put(StringStringMap.Key(MapCursor));
            Put(" : ");
            Put(StringStringMap.Element(MapCursor));
            New_Line;
            MapCursor:=StringStringMap.Next(MapCursor);

         end loop;

         ModuleCursor:=ModuleCursor.Next;
      end loop;
   end Debug;
   ---------------------------------------------------------------------------

   procedure Insert
     (Item : in out Config_Type;
      ModuleName : Unbounded_String;
      Key        : Unbounded_String;
      Value      : Unbounded_String) is

      ModuleMap : access StringStringMap.Map;

   begin
      ModuleMap:=GetModuleMap
        (Item => Item,
         Name => ModuleName);

      if ModuleMap=null then
         NewModule
           (Item => Item,
            Name => ModuleName);
         ModuleMap := GetModuleMap
           (Item => Item,
            Name => ModuleName);
      end if;

      ModuleMap.Insert
        (Key => Key,
         New_Item => Value);

   end Insert;
   ---------------------------------------------------------------------------

   procedure Clear
     (Item : in out Config_Type) is

      ModuleCursor : ModuleAccess;
      ModuleCursor2 : ModuleAccess;

   begin

      ModuleCursor:=Item.First;
      while ModuleCursor/=null loop

         ModuleCursor2:=ModuleCursor.Next;

         Free(ModuleCursor);

         ModuleCursor:=ModuleCursor2;

      end loop;

   end Clear;
   ---------------------------------------------------------------------------

   procedure SaveToFile
     (Item     : in out Config_Type;
      FileName : String) is

      use type StringStringMap.Cursor;

      File         : Ada.Text_IO.File_Type;
      ModuleCursor : ModuleAccess;
      MapCursor    : StringStringMap.Cursor;
      Key          : Unbounded_String;
      Value        : Unbounded_String;

   begin
      Ada.Text_IO.Create
        (File => File,
         Name => FileName);

      ModuleCursor:=Item.First;
      while ModuleCursor/=null loop

         Ada.Text_IO.Unbounded_IO.Put_Line
           (File => File,
            Item => ModuleCursor.Name);

         MapCursor:=ModuleCursor.Map.First;

         while MapCursor/=StringStringMap.No_Element loop

            Key   := StringStringMap.Key(MapCursor);
            Value := StringStringMap.Element(MapCursor);

            if Key="" then
               raise InvalidName;
            end if;

            Ada.Text_IO.Unbounded_IO.Put_Line
              (File => File,
               Item => Key);

            Ada.Text_IO.Unbounded_IO.Put_Line
              (File => File,
               Item => Value);

            MapCursor:=StringStringMap.Next(MapCursor);
         end loop;
         -- Indicate end of map
         Ada.Text_IO.Put_Line
           (File => File,
            Item => "");

         ModuleCursor:=ModuleCursor.Next;
      end loop;
      -- Indicate end of modules
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
      ModuleName : Unbounded_String;
      Key        : Unbounded_String;
      Value      : Unbounded_String;

      Map : access StringStringMap.Map;

   begin
      Ada.Text_IO.Open
        (File => File,
         Name => FileName,
         Mode => Ada.Text_IO.In_File);

      modulloop:
      loop
         Ada.Text_IO.Unbounded_IO.Get_Line
           (File => File,
            Item => ModuleName);
         exit modulloop when ModuleName="";

         NewModule
           (Item => Item,
            Name => ModuleName);

         Map:=GetModuleMap
           (Item => Item,
            Name => ModuleName);

         maploop:
         loop
            Ada.Text_IO.Unbounded_IO.Get_Line
              (File => File,
               Item => Key);
            exit maploop when Key="";
            Ada.Text_IO.Unbounded_IO.Get_Line
              (File => File,
               Item => Value);
            Map.Insert
              (Key      => Key,
               New_Item => Value);
         end loop maploop;

      end loop modulloop;

      Ada.Text_IO.Close
        (File => File);
   end LoadFromFile;
   ---------------------------------------------------------------------------

   procedure NewModule
     (Item : in out Config_Type;
      Name : Unbounded_String) is

      NModul : ModuleAccess;

   begin
      -- TODO: Check for double names.

      if Name="" then
         raise InvalidName;
      end if;
      NModul:=new Module;
      NModul.Name := Name;
      NModul.Last := null;
      NModul.Next := Item.First;
      if Item.First/=null then
         Item.First.Last:=NModul;
      end if;
      Item.First:=NModul;

   end NewModule;
   ---------------------------------------------------------------------------

   function GetModuleMap
     (Item : Config_Type;
      Name : Unbounded_String)
      return access StringStringMap.Map is

      Cursor : ModuleAccess;

   begin
      Cursor := Item.First;
      while (Cursor/=null)
        and then (Cursor.Name/=Name) loop
         Cursor:=Cursor.Next;
      end loop;
      if (Cursor/=null) then
         return Cursor.Map'Access;
      else
         return null;
      end if;
   end GetModuleMap;
   ---------------------------------------------------------------------------

end Config;
