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
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Basics; use Basics;

package body ColorString is

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => ColorString_Type,
      Name   => ColorString_Access);
   ---------------------------------------------------------------------------

   procedure Clear
     (ColorString : in out ColorString_Access) is
   begin

      Free(ColorString);

   end Clear;
   ---------------------------------------------------------------------------

   procedure Append
     (ColorString : in out ColorString_Access;
      String      : Unbounded_String;
      Color       : Color_Type) is

      NewColorString : ColorString_Access;
      OldLength      : Natural;

      UCS4 : Unbounded_Wide_Wide_String;

   begin
      UCS4 := UTF8ToUCS4(String);

      if ColorString/=null then
         OldLength:=ColorString'Last;
         NewColorString:=new ColorString_Type
           (1..ColorString'Last+Length(UCS4));
         NewColorString(1..ColorString'Last):=ColorString.all;
         Free(ColorString);
      else
         OldLength      := 0;
         NewColorString := new ColorString_Type
           (1..Length(UCS4));
      end if;

      for i in 1..Length(String) loop
         declare
            Elem : ColorStringElement_Type renames NewColorString(OldLength+i);
         begin
            Elem.Color := Color;
            Elem.Char  := Element(UCS4,i);
         end;
      end loop;

      ColorString:=NewColorString;

   end Append;
   ---------------------------------------------------------------------------

end Colorstring;
