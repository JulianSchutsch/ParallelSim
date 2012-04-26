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
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Basics; use Basics;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;

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
         Load   : Load_Access;
         Unload : Unload_Access;
         Next   : FontImplementation_Access;
         Last   : FontImplementation_Access;
      end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => FontImplementation_Type,
      Name   => FontImplementation_Access);

   FontImplementations : FontImplementation_Access:=null;
   Fonts               : Font_ClassAccess:=null;
   ---------------------------------------------------------------------------

   function TextWidth
     (Font : access Font_Type'Class;
      Text : Unbounded_String)
      return Integer is

      PreviousCharacter : Wide_Wide_Character:=Wide_Wide_Character'Val(0);
      ThisCharacter : Wide_Wide_Character;
      UCS4  : constant Unbounded_Wide_Wide_String:=UTF8ToUCS4(Text);
      Width : Float:=0.0;

   begin

      for i in 1..Length(UCS4) loop

         ThisCharacter:=Element(UCS4,i);
         Width:=Width+Font_ClassAccess(Font).Kerning
           (FirstChar  => PreviousCharacter,
            SecondChar => ThisCharacter);

         Width:=Width+Font.CharacterWidth(ThisCharacter);

      end loop;

      return Integer(Float'Rounding(Width));

   end;
   ---------------------------------------------------------------------------

   procedure TextOut
     (Font   : access Font_Type;
      Canvas : Standard.Canvas.Canvas_ClassAccess;
      X      : Float;
      Y      : Float;
      Text   : Unbounded_String;
      Color  : Standard.Canvas.Color_Type) is

      XPosition : Float;
      YPosition : Float;

      UCS4              : constant Unbounded_Wide_Wide_String:=UTF8ToUCS4(Text);
      ThisCharacter     : Wide_Wide_Character;
      PreviousCharacter : Wide_Wide_Character:=Wide_Wide_Character'Val(0);

   begin

      XPosition := X;
      YPosition := Y;
      New_Line;

      for i in 1..Length(UCS4) loop

         ThisCharacter:=Element(UCS4,i);

         XPosition:=XPosition+Font_ClassAccess(Font).Kerning
           (FirstChar  => PreviousCharacter,
            SecondChar => ThisCharacter);
         Put("Pos");
         Put(Float'Image(XPosition));
         New_Line;

         Font_ClassAccess(Font).CharacterOut
           (Canvas      => Canvas,
            X           => XPosition,
            Y           => YPosition,
            Char        => ThisCharacter,
            Color       => Color);

         PreviousCharacter:=ThisCharacter;

      end loop;

   end TextOut;
   ---------------------------------------------------------------------------

   procedure Register
     (Load   : Load_Access;
      Unload : Unload_Access) is

      FontImplementation : FontImplementation_Access;

   begin

      FontImplementation:=new FontImplementation_Type;
      FontImplementation.Load   := Load;
      FontImplementation.Unload := Unload;
      FontImplementation.Next   := FontImplementations;
      if FontImplementations/=null then
         FontImplementations.Last:=FontImplementation;
      end if;
      FontImplementations:=FontImplementation;

   end Register;
   ---------------------------------------------------------------------------

   procedure UnRegister
     (Load : Load_Access) is

      Cursor : FontImplementation_Access;

   begin

      Cursor:=FontImplementations;
      while Cursor/=null loop

         if Cursor.Load=Load then

            if Cursor.Last/=null then
               Cursor.Last.Next:=Cursor.Next;
            else
               FontImplementations:=Cursor.Next;
            end if;

            if Cursor.Next/=null then
               Cursor.Next.Last:=Cursor.Last;
            end if;

            Free(Cursor);
            return;

         end if;

         Cursor:=Cursor.Next;

      end loop;

   end UnRegister;

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

         Font:=Implementation.Load
           (Name       => Name,
            Size       => Size,
            Attributes => Attributes);

         if Font/=null then

            Font.Unload := Implementation.Unload;

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
     (Font : in out Font_ClassAccess) is

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

         Free(Font.Attributes);
         Free(Font);

      end if;
   end Release;
   ---------------------------------------------------------------------------

end Fonts;
