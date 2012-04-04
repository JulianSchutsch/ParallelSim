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
--   4.Apr 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

package Fonts.FreeType.Large is

   type LargeFont_Type is new FreeTypeFont_Type with private;
   type LargeFont_Access is access all LargeFont_Type;

private

   type LargeFont_Type is new FreeTypeFont_Type with
      record
         null;
      end record;

   overriding
   procedure CharacterOut
     (Font   : access LargeFont_Type;
      Canvas : Standard.Canvas.Canvas_ClassAccess;
      X      : in out Integer;
      Y      : in out Integer;
      Char   : Wide_Wide_Character;
      Color  : Standard.Canvas.Color_Type);

   overriding
   function CharacterWidth
     (Font : access LargeFont_Type;
      Char : Wide_Wide_Character)
      return Integer;

   overriding
   function Kerning
     (Font       : access LargeFont_Type;
      FirstChar  : Wide_Wide_Character;
      SecondChar : Wide_Wide_Character)
      return Integer;

end Fonts.FreeType.Large;
