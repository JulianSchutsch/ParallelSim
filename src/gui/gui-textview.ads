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

private with ColorString;
with Fonts;

package GUI.TextView is

   type TextView_Type is new Object_Type with private;
   type TextView_Access is access all TextView_Type;
   type TextView_ClassAccess is access all TextView_Type'Class;

   procedure Initialize
     (Item   : TextView_Access;
      Parent : Object_ClassAccess);

   overriding
   procedure Finalize
     (Item : access TextView_Type);

   procedure WriteLine
     (Item   : access TextView_Type;
      String : Unbounded_String;
      Color  : Canvas.Color_Type);

   procedure Clear
     (Item : access TextView_Type);

private

   use ColorString;

   type Line_Type;
   type Line_Access is access Line_Type;

   type Line_Type is
      record
         Content : ColorString_Access;
         Next    : Line_Access:=null;
         Last    : Line_Access:=null;
      end record;

   type CanvasLine_Type;
   type CanvasLine_Access is access CanvasLine_Type;
   type CanvasLine_Type is
      record
         Canvas : Canvas_ClassAccess:=null;
         Next   : CanvasLine_Access:=null;
         Last   : CanvasLine_Access:=null;
      end record;

   type TextView_Type is new Object_Type with
      record
         FirstLine      : Line_Access:=null;
         LastLine       : Line_Access:=null;
         VisibleLines   : CanvasLine_Access:=null;
         Font           : Fonts.Font_ClassAccess:=null;
         SpaceCharWidth : Integer:=10; -- TODO: Obtain it properly
      end record;

end GUI.TextView;
