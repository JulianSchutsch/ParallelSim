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

package body GUI.TextView is

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Line_Type,
      Name   => Line_Access);

   procedure ClearLineBreaks
     (Item : access TextView_Type) is
      Line     : Line_Access;
      NextLine : Line_Access;
   begin

      Line:=Item.FirstLine;
      while Line/=null loop
         NextLine := Line.Next;
         Free(Line);
         Line     := NextLine;
      end loop;

   end;
   ---------------------------------------------------------------------------

   procedure AddLineBreaks
     (Item : access TextView_Type;
      Line : Line_Access) is

      LinePosition : Natural:=1;
      LineStart    : Natural:=1;
      LineWidth    : Integer:=0;
      Str          : Unbounded_String;

      -- This function tries to add as much space as possible to the
      -- current line if any space chars exists at the current LinePosition
      -- It returns true if there are space chars
      function ConsumeSpace
        return Boolean is

      begin
         if (LinePosition<Length(Str))
           and then (Line.Content(LinePosition).Char/=' ') then
            return false;
         end if;
         -- Consume at least the first character
         LineWidth:=LineWidth+Item.SpaceCharWidth;
         LinePosition:=LinePosition+1;

         -- Consume more space characters
         while ((LinePosition<Length(Str)
           and (LineWidth+Item.SpaceCharWidth<Item.Priv.Bounds.Width))
           and then (Line.Content(LinePosition).Char=' ')) loop
            LinePosition:=LinePosition+1;
         end loop;

         return true;

      end ConsumeSpace;
      ------------------------------------------------------------------------

      function ConsumeWord
        return Boolean is
         Pos       : Natural:=LinePosition;
         WordStart : constant Natural:=LinePosition;
         Width     : Integer;
      begin

         while (Pos<Length(Str))
           and then (Line.Content(Pos).Char/=' ') loop
            Pos:=Pos+1;
         end loop;

         if Pos=LinePosition then
            return False;
         else
            Width:=Item.Font.TextWidth
              (Unbounded_Slice(Str,WordStart,Pos-1));

            if LineWidth+Width>Item.Priv.Bounds.Width then
               return False;
            else
               LineWidth    := LineWidth+Width;
               LinePosition := Pos;
               return True;
            end if;
         end if;

      end ConsumeWord;
      ------------------------------------------------------------------------

      procedure ConsumePartWord is

         WordStart : constant Natural:=LinePosition;
         Width     : Integer;

      begin
         if LinePosition<Length(Str) then
            LinePosition:=LinePosition+1;
         end if;

         while (LinePosition<Length(Str))
           and then (Line.Content(LinePosition).Char/=' ') loop

            Width:=Item.Font.TextWidth
              (Unbounded_Slice(Str,WordStart,LinePosition));
            if LineWidth+Width>Item.Priv.Bounds.Width then
               return;
            end if;

           LinePosition:=LinePosition+1;
         end loop;

      end ConsumePartWord;
      ------------------------------------------------------------------------

      procedure ConsumeWords is
      begin
         loop
            exit when not ConsumeWord;
            exit when not ConsumeSpace;
         end loop;
      end ConsumeWords;
      ------------------------------------------------------------------------

   begin

      if Line=null then
         return;
      end if;
      Str:=ToUnboundedString(Line.Content);

      while LinePosition<Line.Content'Last loop

         if ConsumeSpace then
            ConsumeWords;
         else
            if ConsumeWord then
               if ConsumeSpace then
                  ConsumeWords;
               end if;
            else
               ConsumePartWord;
            end if;
         end if;

         Line.Content(LineStart).NextLine:=LinePosition;

         LineStart:=LinePosition;

      end loop;

   end AddLineBreaks;
   ---------------------------------------------------------------------------

   procedure CalculateLineBreaks
     (Item : access TextView_Type) is

      Line : Line_Access;

   begin
      ClearLineBreaks(Item);
      Line:=Item.FirstLine;
      while Line/=null loop
         AddLineBreaks
           (Item => Item,
            Line => Line);
         Line:=Line.Next;
      end loop;

   end CalculateLineBreaks;
   ---------------------------------------------------------------------------

   procedure Clear
     (Item : access TextView_Type) is

      Line     : Line_Access;
      NextLine : Line_Access;

   begin

      Line:=Item.FirstLine;
      while Line/=null loop
         NextLine := Line.Next;
         Free(Line);
         Line     := NextLine;
      end loop;

      CalculateLineBreaks(Item);

   end Clear;
   ---------------------------------------------------------------------------

   procedure WriteLine
     (Item   : access TextView_Type;
      String : Unbounded_String;
      Color  : Canvas.Color_Type) is

      NewLine : Line_Access;
   begin

      NewLine:=new Line_Type;
      ColorString.Append
        (ColorString => NewLine.Content,
         String      => String,
         Color       => Color);

      NewLine.Last:=Item.LastLine;
      if Item.LastLine/=null then
         Item.LastLine.Next:=NewLine;
      else
         Item.FirstLine:=NewLine;
      end if;
      Item.LastLine:=NewLine;

   end WriteLine;
   ---------------------------------------------------------------------------

   procedure Initialize
     (Item   : TextView_Access;
      Parent : Object_ClassAccess) is
   begin

      GUI.Initialize
        (Item   => Object_Access(Item),
         Parent => Parent);

   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize
     (Item : access TextView_Type) is
   begin

      GUI.Finalize
        (Item => Object_Access(Item));

   end Finalize;
   ---------------------------------------------------------------------------

end GUI.TextView;
