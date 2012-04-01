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

pragma Ada_2005;
with Ada.Unchecked_Deallocation;

package body ColorString is

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => ColorStringArray_Type,
      Name   => ColorStringArray_Access);
   ---------------------------------------------------------------------------

   procedure Clear
     (ColorString : in out ColorString_Type) is
   begin
      Free(ColorString.Content);
   end Clear;
   ---------------------------------------------------------------------------

   procedure Append
     (ColorString : in out ColorString_Type;
      String      : Unbounded_String;
      Color       : Color_Type) is

      NewColorString : ColorStringArray_Access;

   begin

      NewColorString:=new ColorStringArray_Type
        (1..ColorString.Content'Last+Length(String));
      NewColorString(1..ColorString.Content'Last):=ColorString.Content.all;

      for i in 1..Length(String) loop
         declare
            Elem : ColorStringElement_Type renames NewColorString(ColorString.Content'Last+i);
         begin
            Elem.Color := Color;
            Elem.Char  := Element(String,i);
         end;
      end loop;

   end Append;
   ---------------------------------------------------------------------------

end Colorstring;
