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
--   29.Mar 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Canvas;
with ColorString;

package Fonts is

   FailedFontImplementationInitialization : Exception;
   FailedRendering                        : Exception;

   type Font_Type is abstract tagged private;

   function TextWidth
     (Font : access Font_Type;
      Text : Unbounded_String)
      return Integer is abstract;

   function TextWidth
     (Font : access Font_Type;
      Text : ColorString.ColorString_Type)
      return Integer is abstract;

   procedure TextOut
     (Font   : access Font_Type;
      Canvas : Standard.Canvas.BasicCanvas_ClassAccess;
      X      : Integer;
      Y      : Integer;
      Text   : Unbounded_String;
      Color  : Standard.Canvas.Color_Type) is abstract;

   procedure TextOut
     (Font   : access Font_Type;
      Canvas : Standard.Canvas.BasicCanvas_ClassAccess;
      X      : Integer;
      Y      : Integer;
      Text   : ColorString.ColorString_Type) is abstract;

   type Font_ClassAccess is access all Font_Type'Class;

   type Attributes_Type is array(Natural range <>) of Unbounded_String;
   type Attributes_Access is access Attributes_Type;

   NoAttributes : Attributes_Type(0..-1);

   procedure Release
     (Font : Font_ClassAccess);

   function Lookup
     (Name       : Unbounded_String;
      Size       : Natural;
      Attributes : Attributes_Type)
      return Font_ClassAccess;

   type Load_Access is
     access function
       (Name       : Unbounded_String;
        Size       : Natural;
        Attributes : Attributes_Type)
        return Font_ClassAccess;

   procedure Register
     (Load : Load_Access);

   procedure UnRegister
     (Load : Load_Access);

private

   FontNotFound : Exception;

   type Font_Type is abstract tagged
      record
         ReferenceCount : Natural := 1;
         Name           : Unbounded_String;
         Size           : Natural;
         Attributes     : Attributes_Access;
         Next           : Font_ClassAccess;
         Last           : Font_ClassAccess;
      end record;

end Fonts;
