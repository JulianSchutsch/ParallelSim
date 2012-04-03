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

-- Revision History
--   1.Apr 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

with Canvas; use Canvas;

package Fonts.ColorStrings is

   type ColorStringElement_Type is
      record
         Char       : Wide_Wide_Character;
         Color      : Color_Type;
         NextLine   : Natural;
         Modified   : Boolean;
         LineWidth  : Integer;
         AccumWidth : Integer;
      end record;

   type ColorString_Type is array (Natural range <>) of ColorStringElement_Type;
   type ColorString_Access is access ColorString_Type;

   procedure Clear
     (ColorString : in out ColorString_Access);

   procedure Append
     (ColorString : in out ColorString_Access;
      String      : Unbounded_String;
      Color       : Color_Type);

end Fonts.ColorStrings;
