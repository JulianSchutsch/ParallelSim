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

private with Fonts.ColorStrings;
with Fonts;

package GUI.TextView is

   NoFontSelected : Exception;

   type OnWrappedLineCountChange_Access is
     access procedure
       (LineCount : Integer);

   type TextView_Public is new Object_Type with
      record
         OnWrappedLineCountChange : OnWrappedLineCountChange_Access:=null;
      end record;

   type TextView_Type is new TextView_Public with private;

   type TextView_Access is access all TextView_Type;
   type TextView_ClassAccess is access all TextView_Type'Class;

   type Line_Type is private;
   type Line_Access is access Line_Type;

   procedure Initialize
     (Item   : TextView_Access;
      Parent : Object_ClassAccess);

   overriding
   procedure Finalize
     (Item : access TextView_Type);

   overriding
   procedure Resize
     (Item : access TextView_Type);

   overriding
   function MouseDown
     (Item   : access TextView_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer)
      return Boolean;

   procedure WriteLine
     (Item   : access TextView_Type;
      String : Unbounded_String;
      Color  : Canvas.Color_Type);

   function NewLine
     (Item : access TextView_Type;
      String : Unbounded_String;
      Color : Canvas.Color_Type)
      return Line_Access;

   procedure InsertBefore
     (Item   : access TextView_Type;
      Line   : Line_Access;
      String : Unbounded_String;
      Color  : Canvas.Color_Type);

   function InsertCharacters
     (Item     : access TextView_Type;
      Line     : Line_Access;
      Position : Natural;
      String   : Unbounded_String;
      Color    : Canvas.Color_Type)
      return Integer;

   -- Scrolls to make the specified position visible
   procedure MakeVisible
     (TextView : access TextView_Type;
      Line     : Line_Access;
      Position : Integer);

   procedure Clear
     (Item : access TextView_Type);

   procedure SetFont
     (Item : access TextView_Type;
      Font : Fonts.Font_ClassAccess);

private

   use Fonts.ColorStrings;

   type Line_Type is new Fonts.ColorStrings.ColorString_Type with
      record
         Next     : Line_Access:=null;
         Last     : Line_Access:=null;
      end record;

   type CanvasLine_Type;
   type CanvasLine_Access is access CanvasLine_Type;
   type CanvasLine_Type is
      record
         WrappedLine : Natural;
         Line        : Line_Access;
         Canvas      : Canvas_ClassAccess := null;
         Next        : CanvasLine_Access  := null;
         Last        : CanvasLine_Access  := null;
      end record;

   type TextView_Type is new TextView_Public with
      record
         FirstLine              : Line_Access:=null;
         LastLine               : Line_Access:=null;
         CanvasLines            : CanvasLine_Access:=null;
         Font                   : Fonts.Font_ClassAccess:=null;
         SpaceCharWidth         : Integer;
         LineHeight             : Integer;
         FirstWrappedLine       : Natural := 0;
         InWrappedLinePosition  : Natural := 0;
      end record;

end GUI.TextView;
