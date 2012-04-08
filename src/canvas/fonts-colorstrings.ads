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

with Canvas;

package Fonts.ColorStrings is

   FontNotAssigned    : Exception;
   InvalidWrappedLine : Exception;
   IndexOutOfRange    : Exception;

   type ColorStringElement_Type is private;

   type ColorStringArray_Type is array (Natural range <>) of ColorStringElement_Type;
   type ColorStringArray_Access is access ColorStringArray_Type;

   type ColorString_Type is tagged
      record
         WrappedLineCount   : Natural:=0;
         CurrentWidth       : Integer:=0;
         CurrentWrappedLine : Natural:=0;
         CurrentPosition    : Natural:=0;
         Font               : Font_ClassAccess:=null;
         Content            : ColorstringArray_Access:=null;
      end record;

   procedure Render
     (ColorString     : in out Colorstring_Type;
      Canvas          : Standard.Canvas.Canvas_ClassAccess;
      X               : Integer;
      Y               : Integer);

   procedure GreedyWrapping
     (ColorString : in out ColorString_Type;
      Width       : Integer);

   procedure SelectWrappedLine
     (ColorString : in out ColorString_Type;
      WrappedLine : Natural);

   function FirstWrappedLine
     (ColorString : access ColorString_Type)
      return Boolean;

   function NextWrappedLine
     (ColorString : access ColorString_Type)
      return Boolean;

   procedure Clear
     (ColorString : in out ColorString_Type);

   procedure Reinitialize
     (ColorString : in out ColorString_Type;
      Font        : Font_ClassAccess);

   procedure Initialize
     (ColorString : in out ColorString_Type;
      String      : Unbounded_String;
      Color       : Canvas.Color_Type;
      Font        : Font_ClassAccess);

   -- The position is in wide_wide_char units
   -- The length of the input string in wide_wide_chars is returned
   function Insert
     (ColorString : access ColorString_Type;
      Position    : Integer;
      String      : Unbounded_String;
      Color       : Canvas.Color_Type)
      return Integer;

   function GetWrappedLine
     (ColorString : access ColorString_Type;
      Position    : Integer)
      return Integer;

private
   type ColorStringElement_Type is
      record
         Char       : Wide_Wide_Character;
         Color      : Canvas.Color_Type;
         NextLine   : Natural;
         Modified   : Boolean;
         LineWidth  : Integer;
         AccumWidth : Integer;
      end record;

end Fonts.ColorStrings;
