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

with Interfaces;

package Canvas is

   type Color_Type is new Interfaces.Unsigned_32;

   type Image_Type is array(Natural range <>,Natural range <>) of Color_Type;
   pragma Convention(C,Image_Type);

   type Image_Access is access all Image_Type;

   type BasicCanvas_Type is tagged
      record
         Image         : Image_Access := null;
         Modified      : Boolean      := True;
         ContentHeight : Natural;
         ContentWidth  : Natural;
      end record;
   type BasicCanvas_ClassAccess is access all BasicCanvas_Type'Class;

   procedure GetPixel
     (Canvas : in out BasicCanvas_Type;
      X      : Integer;
      Y      : Integer;
      Color  : out Color_Type);

   procedure SetPixel
     (Canvas : in out BasicCanvas_Type;
      X      : Integer;
      Y      : Integer;
      Color  : Color_Type);

   procedure Clear
     (Canvas : in out BasicCanvas_Type;
      Color  : Color_Type);

   procedure VertLine
     (Canvas : in out BasicCanvas_Type;
      X      : Integer;
      Y      : Integer;
      Height : Integer;
      Color  : Color_Type);

   procedure HorzLine
     (Canvas : in out BasicCanvas_Type;
      X      : Integer;
      Y      : Integer;
      Width  : Integer;
      Color  : Color_Type);

   procedure Bar
     (Canvas : in out BasicCanvas_Type;
      X      : Integer;
      Y      : Integer;
      Height : Integer;
      Width  : Integer;
      Color  : Color_Type);

   function MultiplyAlpha
     (Color : Color_Type;
      Alpha : Integer)
      return Color_Type;

   function PreBlendMix
     (BackgroundColor : Color_Type;
      ForegroundColor : Color_Type)
      return Color_Type;

end Canvas;
