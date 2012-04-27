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
--   27.Apr 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

package GUI.ListBox is

   type ListBox_Type is new GUI.Object_Type with private;
   type ListBox_Access is access all ListBox_Type;
   type ListBox_ClassAccess is access all ListBox_Type'Class;

   overriding
   procedure Finalize
     (Item : access ListBox_Type);

   procedure AddEntry
     (Item   : access ListBox_Type;
      String : Unbounded_String;
      Color  : Canvas.Color_Type) is null;

   procedure ClearEntries
     (Item : access ListBox_Type) is null;

   type ListBox_Constructor is
     access function
       (Parent : GUI.Object_ClassAccess)
        return ListBox_ClassAccess;

private

   type ListBox_Type is new GUI.Object_Type with
      record
         null;
      end record;

end GUI.ListBox;
