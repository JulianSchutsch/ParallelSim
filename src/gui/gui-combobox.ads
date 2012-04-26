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
--   26.Apr 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

with Ada.Containers.Doubly_Linked_Lists;
with Canvas;

package GUI.Combobox is

   type ComboboxEntry_Cursor is private;

   type Combobox_Type is new Object_Type with private;
   type Combobox_ClassAccess is access Combobox_Type'Class;

   procedure AddEntry
     (Item  : access Combobox_Type;
      Text  : Unbounded_String;
      Color : Canvas.Color_Type);

   procedure RemoveEntry
     (Item     : access Combobox_Type;
      Position : ComboboxEntry_Cursor);

private

   type ComboboxEntry_Type;
   type ComboboxEntry_Access is access ComboboxEntry_Type;
   type ComboboxEntry_Cursor is access ComboboxEntry_Type;

   type ComboboxEntry_Type is
      record
         Text  : Unbounded_String;
         Color : Canvas.Color_Type;
         Last  : ComboboxEntry_Access:=null;
         Next  : ComboboxEntry_Access:=null;
      end record;

   type Combobox_Type is new Object_Type with
      record
         FirstEntry : ComboboxEntry_Access:=null;
         LastEntry  : ComboboxEntry_Access:=null;
      end record;

end GUI.Combobox;
