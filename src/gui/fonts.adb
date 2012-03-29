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

pragma Ada_2005;

with Ada.Unchecked_Deallocation;

package body Fonts is

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Font_Type'Class,
      Name   => Font_ClassAccess);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Attributes_Type,
      Name   => Attributes_Access);

   type FontImplementation_Type;
   type FontImplementation_Access is access FontImplementation_Type;

   type FontImplementation_Type is
      record
         LookupFunction : LookupFunction_Access;
         Next           : FontImplementation_Access;
         Last           : FontImplementation_Access;
      end record;

   FontImplementations : FontImplementation_Access:=null;
   Fonts               : Font_ClassAccess:=null;

   procedure Register
     (LookupFunction : LookupFunction_Access) is

      FontImplementation : FontImplementation_Access;

   begin

      FontImplementation:=new FontImplementation_Type;
      FontImplementation.LookupFunction:=LookupFunction;
      FontImplementation.Next:=FontImplementations;
      if FontImplementations/=null then
         FontImplementations.Last:=FontImplementation;
      end if;
      FontImplementations:=FontImplementation;

   end Register;
   ---------------------------------------------------------------------------

   function Lookup
     (Name       : Unbounded_String;
      Size       : Natural;
      Attributes : Attributes_Type)
      return Font_ClassAccess is

      Font : Font_ClassAccess;

      Implementation : FontImplementation_Access;

   begin
      -- Search in the list of allready used fonts for a match
      Font:=Fonts;
      while Font/=null loop

         if (Name=Font.Name)
           and (Size=Font.Size)
           and (Attributes=Font.Attributes.all) then

            Font.ReferenceCount:=Font.ReferenceCount+1;
            return Font;

         end if;

         Font:=Font.Next;
      end loop;

      -- Ask each registered implementation for a match
      Implementation:=FontImplementations;
      while Implementation/=null loop

         Font:=Implementation.LookupFunction
           (Name       => Name,
            Size       => Size,
            Attributes => Attributes);

         if Font/=null then

            Font.Name := Name;
            Font.Size := Size;
            Font.Attributes:= new Attributes_Type(Attributes'Range);
            Font.Attributes.all:= Attributes;
            Font.Next:=Fonts;
            if Fonts/=null then
               Fonts.Last:=Font;
            end if;
            Fonts:=Font;

            return Font;
         end if;

         Implementation:=Implementation.Next;
      end loop;

      raise FontNotFound;

   end Lookup;
   ---------------------------------------------------------------------------

   procedure Release
     (Font : Font_ClassAccess) is

      FontVar : Font_ClassAccess;

   begin
      Font.ReferenceCount:=Font.ReferenceCount-1;
      if Font.ReferenceCount=0 then

         if Font.Next/=null then
            Font.Next.Last:=Font.Last;
         end if;
         if Font.Last/=null then
            Font.Last.Next:=Font.Next;
         else
            Fonts:=Font.Next;
         end if;

         FontVar:=Font;
         Free(Font.Attributes);
         Free(FontVar);

      end if;
   end Release;
   ---------------------------------------------------------------------------

end Fonts;
