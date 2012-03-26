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
--   26.Mar 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

package GUI.Window is

   type Window_Type is new Object_Type with
      record
         null;
      end record;

   type Window_Access is access all Window_Type;
   type Window_ClassAccess is access all Window_Type'Class;

   type Window_Constructor is
     access function
       (Parent : Object_ClassAccess)
        return Window_ClassAccess;

   procedure Initialize
     (Item    : Window_Access;
      Parent  : Object_ClassAccess);

   overriding
   procedure Finalize
     (Item : access Window_Type);

end GUI.Window;
