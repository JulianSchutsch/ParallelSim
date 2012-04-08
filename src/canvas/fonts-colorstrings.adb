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
--with Ada.Text_IO; use Ada.Text_IO;
--with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Fonts.ColorStrings is

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => ColorStringArray_Type,
      Name   => ColorStringArray_Access);
   ---------------------------------------------------------------------------

   function GetWrappedLine
     (ColorString : access ColorString_Type;
      Position    : Integer)
      return Integer is

      LinePosition : Integer;
      LineNumber   : Integer:=0;

   begin
      LinePosition:=ColorString.Content'First;

      while LinePosition<=ColorString.Content'Last loop

         if Position in LinePosition..ColorString.Content(LinePosition).NextLine-1 then
            return LineNumber;
         end if;

         LinePosition := ColorString.Content(LinePosition).NextLine;
         LineNumber   := LineNumber+1;

      end loop;

      return LineNumber;

   end GetWrappedLine;
   ---------------------------------------------------------------------------

   procedure Render
     (ColorString     : in out Colorstring_Type;
      Canvas          : Standard.Canvas.Canvas_ClassAccess;
      X               : Integer;
      Y               : Integer) is

      XPosition : Integer:=X;
      YPosition : Integer:=Y;

   begin

      if ColorString.Font=null then
         raise FontNotAssigned;
      end if;

      for i in ColorString.CurrentPosition
        ..ColorString.Content(ColorString.CurrentPosition).NextLine-1 loop

         ColorString.Font.CharacterOut
           (Canvas => Canvas,
            X      => XPosition,
            Y      => YPosition,
            Char   => ColorString.Content(i).Char,
            Color  => ColorString.Content(i).Color);

      end loop;

   end Render;
   ---------------------------------------------------------------------------

   procedure GreedyWrapping
     (ColorString : in out ColorString_Type;
      Width       : Integer) is

      InWord              : Boolean:=False;
      WordStart           : Integer:=0;
      LineAccumWidth      : Integer:=0;
      LineStart           : Natural:=ColorString.Content'First;
      LineNext            : Natural;
      LineWordCount       : Integer:=0;
      Position            : Natural:=ColorString.Content'First;
      Overflow            : Boolean;

      procedure NewLine is
      begin

         ColorString.WrappedLineCount:=ColorString.WrappedLineCount+1;

         if ColorString.Content(LineStart).NextLine/=Position then

            ColorString.Content(LineStart).Modified  := True;
            ColorString.Content(LineStart).NextLine  := Position;
            ColorString.Content(LineStart).LineWidth
              := ColorString.Content(Position-1).AccumWidth-LineAccumWidth;

         end if;


         LineStart      := Position;
         LineAccumWidth := ColorString.Content(Position-1).AccumWidth;
         LineWordCount  := 0;
         InWord         := False;

      end NewLine;
      ------------------------------------------------------------------------

      procedure StopLine is
      begin

         if Position/=LineStart then
            NewLine;
         end if;

         if ColorString.WrappedLineCount=0 then
            ColorString.WrappedLineCount:=1;
         end if;

      end StopLine;
      ------------------------------------------------------------------------

   begin

      ColorString.WrappedLineCount:=0;

      while Position<=ColorString.Content'Last loop

         Overflow
           :=(ColorString.Content(Position).AccumWidth
              -LineAccumWidth>Width);

         if not InWord then

            if ColorString.Content(Position).Char=' ' then

               if (Position/=LineStart)
                 and Overflow then
                  NewLine;
               end if;

            else

               if (Position/=LineStart)
                 and Overflow then
                  Position:=Position-1;
                  NewLine;
               end if;

               InWord        := True;
               LineWordCount := LineWordCount+1;
               WordStart     := Position;

            end if;

         else

            if ColorString.Content(Position).Char=' ' then

               InWord := False;

               if Overflow then
                  NewLine;
               end if;

            else

               if Overflow then

                  if LineWordCount>1 then
                     Position:=WordStart;
                     InWord := True;
                  end if;

                  NewLine;

               end if;
            end if;

         end if;

         Position:=Position+1;

      end loop;

      StopLine;

      -- TODO: Check if this is even possible (Range /=0)
      Position:=ColorString.Content'First+1;
      LineNext:=ColorString.Content(ColorString.Content'First).NextLine;

      while Position<=ColorString.Content'Last loop
         if Position/=LineNext then
            ColorString.Content(Position).NextLine:=0;
         else
            LineNext:=ColorString.Content(Position).NextLine;
         end if;
         Position:=Position+1;
      end loop;

   end GreedyWrapping;
   ---------------------------------------------------------------------------

   function FirstWrappedLine
     (ColorString : access ColorString_Type)
      return Boolean is
   begin

      if (ColorString.Content/=null) then

         ColorString.CurrentPosition    := ColorString.Content'First;
         ColorString.CurrentWrappedLine := 0;
         ColorString.CurrentWidth
           := ColorString.Content(ColorString.CurrentPosition).LineWidth;
         return True;

      end if;

      return False;

   end;
   ---------------------------------------------------------------------------

   function NextWrappedLine
     (ColorString : access ColorString_Type)
      return Boolean is
   begin

      ColorString.CurrentWrappedLine:=ColorString.CurrentWrappedLine+1;

      if ColorString.CurrentWrappedLine=ColorString.WrappedLineCount then
         ColorString.CurrentWrappedLine := Integer'Last;
         ColorString.CurrentPosition    := Integer'Last;
         return False;
      end if;

      ColorString.CurrentPosition
        :=ColorString.Content(ColorString.CurrentPosition).NextLine;
      ColorString.CurrentWidth
        := ColorString.Content(ColorString.CurrentPosition).LineWidth;

      return True;

   end NextWrappedLine;
   ---------------------------------------------------------------------------

   procedure SelectWrappedLine
     (ColorString : in out ColorString_Type;
      WrappedLine : Natural) is

   begin

      if WrappedLine>=ColorString.WrappedLineCount then
         raise InvalidWrappedLine;
      end if;

      ColorString.CurrentPosition:=ColorString.Content'First;

      for i in 1..WrappedLine loop
         ColorString.CurrentPosition
           :=ColorString.Content(ColorString.CurrentPosition).NextLine;
      end loop;

      ColorString.CurrentWrappedLine:=WrappedLine;
      ColorString.CurrentWidth
        := ColorString.Content(ColorString.CurrentPosition).LineWidth;

   end SelectWrappedLine;
   ---------------------------------------------------------------------------


   procedure CalculateDimensions
     (ColorString : in out ColorString_Type) is

      AccumWidth   : Integer:=0;
      PreviousChar : Wide_Wide_Character:=Wide_Wide_Character'Val(0);
      ThisChar     : Wide_Wide_Character;

   begin

      if ColorString.Font=null then
         return;
      end if;

      for i in ColorString.Content'Range loop

         ThisChar             := ColorString.Content(i).Char;

         AccumWidth
           := AccumWidth+ColorString.Font.CharacterWidth(ThisChar);

         ColorString.Content(i).AccumWidth := AccumWidth;
         ColorString.Content(i).NextLine := 0; -- Reset this to reset wrapping

         AccumWidth
           := AccumWidth+ColorString.Font.Kerning(PreviousChar,ThisChar);

         PreviousChar:=ThisChar;


      end loop;

   end CalculateDimensions;
   ---------------------------------------------------------------------------

   procedure Clear
     (ColorString : in out ColorString_Type) is
   begin

      if ColorString.Content/=null then
         Free(ColorString.Content);
      end if;

   end Clear;
   ---------------------------------------------------------------------------

   procedure Reinitialize
     (ColorString : in out ColorString_Type;
      Font        : Font_ClassAccess) is
   begin
      ColorString.Font:=Font;
      CalculateDimensions(ColorString);
   end Reinitialize;
   ---------------------------------------------------------------------------

   function Insert
     (ColorString : access ColorString_Type;
      Position    : Integer;
      String      : Unbounded_String;
      Color       : Canvas.Color_Type)
      return Integer is

      UCS4       : Unbounded_Wide_Wide_String;
      NewContent : ColorStringArray_Access;

   begin

      UCS4:=UTF8ToUCS4(String);

      if ColorString.Content=null then
         if Position=1 then
            -- Simply create the entire thing

            ColorString.Initialize
              (String => String,
               Color  => Color,
               Font   => ColorString.Font);

            return Length(UCS4);

         end if;
         raise IndexOutOfRange;
      end if;

      if Position not in ColorString.Content'First..ColorString.Content'Last+1 then
         raise IndexOutOfRange;
      end if;

      NewContent:=new ColorStringArray_Type
        (1..ColorString.Content'Last+Length(UCS4));

      for i in 1..Position-1 loop
         NewContent(i):=ColorString.Content(i);
      end loop;

      for i in 1..Length(UCS4) loop
         NewContent(Position+i-1).Char  := Element(UCS4,i);
         NewContent(Position+i-1).Color := Color;
      end loop;

      for i in Position..ColorString.Content'Last loop
         NewContent(i+Length(UCS4)):=ColorString.Content(i);
      end loop;

      Free(ColorString.Content);

      ColorString.Content:=NewContent;

      CalculateDimensions(ColorString.all);

      return Length(UCS4);

   end Insert;
   ---------------------------------------------------------------------------

   procedure Initialize
     (ColorString : in out ColorString_Type;
      String      : Unbounded_String;
      Color       : Canvas.Color_Type;
      Font        : Font_ClassAccess) is

      UCS4 : Unbounded_Wide_Wide_String;

   begin

      ColorString.Font:=Font;

      UCS4 := UTF8ToUCS4(String);

      if ColorString.Content/=null then
         Free(Colorstring.Content);
      end if;

      ColorString.Content := new ColorStringArray_Type
         (1..Length(UCS4));

      for i in 1..Length(String) loop
         ColorString.Content(i).Color := Color;
         Colorstring.Content(i).Char  := Element(UCS4,i);
      end loop;

      CalculateDimensions(ColorString);

   end Initialize;
   ---------------------------------------------------------------------------

end Fonts.Colorstrings;
