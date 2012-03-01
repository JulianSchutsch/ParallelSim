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

with Ada.Containers.Doubly_Linked_Lists;
with Basics; use Basics;
with ProgramArguments;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;

package body Network.Config is

   package StreamImplementationList_Pack is
     new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => StreamImplementation_Type,
      "=" => "=");

   StreamImplementationList : StreamImplementationList_Pack.List;

   function FindStreamImplementation
     (Identifier : Unbounded_String)
      return StreamImplementation_Type is

      use type StreamImplementationList_Pack.Cursor;

      Cursor : StreamImplementationList_Pack.Cursor;
      Implementation : StreamImplementation_Type;

   begin
      Cursor:=StreamImplementationList.First;

      while Cursor/=StreamImplementationList_Pack.No_Element loop

         Implementation:=StreamImplementationList_Pack.Element(Cursor);
         if Implementation.ImplementationIdentifier=Identifier then
            return Implementation;
         end if;
         Cursor:=StreamImplementationList_Pack.Next(Cursor);

      end loop;

      raise ImplementationNotFound with "Stream";
   end FindStreamImplementation;
   ---------------------------------------------------------------------------

   procedure RegisterStreamImplementation
     (StreamImplementation : StreamImplementation_Type) is
   begin
      StreamImplementationList.Append
        (New_Item => StreamImplementation);
   end;
   ---------------------------------------------------------------------------

   package ProcessesImplementationList_Pack is
     new Ada.Containers.Doubly_Linked_Lists
       (Element_Type => ProcessesImplementation_Type,
        "="          => "=");

   ProcessesImplementationList : ProcessesImplementationList_Pack.List;

   function FindProcessesImplementation
     (Identifier : Unbounded_String)
      return ProcessesImplementation_Type is

      use type ProcessesImplementationList_Pack.Cursor;

      Cursor : ProcessesImplementationList_Pack.Cursor;
      Implementation : ProcessesImplementation_Type;

   begin

      Put(Identifier);

      Cursor:=ProcessesImplementationList.First;

      while Cursor/=ProcessesImplementationList_Pack.No_Element loop

         Implementation:=ProcessesImplementationList_Pack.Element(Cursor);
         Put("Implementation Id:");
         Put(To_String(Implementation.ImplementationIdentifier));
         New_Line;
         Put("Test ID:");
         Put(To_String(Identifier));
         New_Line;
         New_Line;
         if Implementation.ImplementationIdentifier=Identifier then
            return Implementation;
         end if;
         Cursor:=ProcessesImplementationList_Pack.Next(Cursor);

      end loop;

      raise ImplementationNotFound with "Processes";
   end FindProcessesImplementation;
   ---------------------------------------------------------------------------

   procedure RegisterProcessImplementation
     (ProcessesImplementation : ProcessesImplementation_Type) is
   begin
      Put("Register Process Implementation");
      ProcessesImplementationList.Append
        (New_Item => ProcessesImplementation);
      Put("Done");
      New_Line;
   end RegisterProcessImplementation;
   ---------------------------------------------------------------------------

   function FindImplementation
     (Configuration : Config_Type;
      ModuleName    : Unbounded_String)
      return Implementation_Type is

      use type StringStringMap.Cursor;

      Impl : Implementation_Type;
      NetworkMap : access StringStringMap.Map;
      Cursor : StringStringMap.Cursor;

   begin

      NetworkMap:=GetModuleMap
        (Item => Configuration,
         Name => ModuleName);

      if NetworkMap=null then
         raise InvalidConfiguration with "Missing Modul";
      end if;

      Cursor := StringStringMap.Find
        (Container => NetworkMap.all,
         Key       => To_Unbounded_String("StreamImplementation"));

      if Cursor=StringStringMap.No_Element then
         raise InvalidConfiguration with "Missing:StreamImplementation";
      end if;

      Impl.Streams:=FindStreamImplementation
        (StringStringMap.Element(Cursor));

      Cursor :=StringStringMap.Find
        (Container => NetworkMap.all,
         Key       => To_Unbounded_String("ProcessesImplementation"));

      if Cursor=StringStringMap.No_Element then
         raise InvalidConfiguration with "Missing:ProcessesImplementation";
      end if;

      Impl.Processes:=FindProcessesImplementation
        (StringStringMap.Element(Cursor));
      return Impl;
   end;
   ---------------------------------------------------------------------------

   procedure LoadConfiguration
     (Configuration : in out Config_Type) is

      use type StringStringMap.Cursor;

      ProcessesImplementation : ProcessesImplementation_Type;
      Cursor                  : StringStringMap.Cursor;

   begin

      Cursor := ProgramArguments.VariablesMap.Find
        (Key => To_Unbounded_String("Network.Processes"));

      if Cursor=StringStringMap.No_Element then
         raise ConfigurationTypeParameterMissing;
      end if;

      Put("LOADCONFIG WITH ");
      Put(To_String(StringStringMap.Element(Cursor)));
      New_Line;

      ProcessesImplementation:=FindProcessesImplementation
        (Identifier => StringStringMap.Element(Cursor));

      ProcessesImplementation.LoadConfig
        (Configuration => Configuration);

   end LoadConfiguration;
   ---------------------------------------------------------------------------

end Network.Config;
