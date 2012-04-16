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

   type WindowChangeMode_Enum is
     (WindowChangeModeNothing,
      WindowChangeModeMove,
      WindowChangeModeSizeTopLeft,
      WindowChangeModeSizeTop,
      WindowChangeModeSizeTopRight,
      WindowChangeModeSizeLeft,
      WindowChangeModeSizeRight,
      WindowChangeModeSizeBottomLeft,
      WindowChangeModeSizeBottom,
      WindowChangeModeSizeBottomRight);

   type Window_Type is new Object_Type with private;
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

   overriding
   procedure Focus
     (Item : access Window_Type);

   procedure StartChange
     (Window : access Window_Type;
      Refx   : Integer;
      Refy   : Integer;
      Mode   : WindowChangeMode_Enum);

   procedure ApplyChange
     (Window : access Window_Type;
      RefX   : Integer;
      RefY   : Integer);

   procedure StopChange
     (Window : access Window_Type);

private

   type Window_Type is new Object_Type with
      record
         RefX      : Integer;
         RefY      : Integer;
         RefHeight : Integer;
         RefWidth  : Integer;
         Mode      : WindowChangeMode_Enum:=WindowChangeModeNothing;
      end record;

end GUI.Window;
