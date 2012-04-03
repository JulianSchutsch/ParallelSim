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
with Ada.Text_IO; use Ada.Text_IO;
with Canvas;

package body GUI.TextView is

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Line_Type,
      Name   => Line_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => CanvasLine_Type,
      Name   => CanvasLine_Access);

   procedure FreeCanvasLine
     (Item       : access TextView_Type;
      CanvasLine : in out CanvasLine_Access) is
   begin

      if CanvasLine.Canvas/=null then
         Item.Context.FreeCanvas(CanvasLine.Canvas);
      end if;

      Free(CanvasLine);

   end FreeCanvasLine;
   ---------------------------------------------------------------------------

   procedure ClearCanvasLines
     (Item     : access TextView_Type) is

      Line     : CanvasLine_Access;
      NextLine : CanvasLine_Access;

   begin
      Put("ClearCanvasLines");

      Line:=Item.CanvasLines;
      while Line/=null loop

         NextLine:=Line.Next;

         FreeCanvasLine
           (Item       => Item,
            CanvasLine => Line);

         Line:=NextLine;

      end loop;

      Item.CanvasLines:=null;

   end ClearCanvasLines;
   ---------------------------------------------------------------------------

   function SelectSubLine
     (Line    : Line_Access;
      SubLine : Natural)
      return Natural is

      CurrentPosition : Integer:=1;

   begin
      -- There is no check here wether or not the current position is
      -- valid or not. It should not be necessary since Ada throws an
      -- exception for invalid access to Line.Content here.
      for i in 1..SubLine loop
         CurrentPosition:=Line.Content(CurrentPosition).NextLine;
      end loop;
      return CurrentPosition;
   end SelectSubLine;
   ---------------------------------------------------------------------------

   procedure SelectLine
     (Item            : access TextView_Type;
      VisibleLine     : Natural;
      Line            : out Line_Access;
      SubLinePosition : out Natural) is

      CurrentLine        : Line_Access;
      CurrentVisibleLine : Integer:=0;

   begin

      CurrentLine:=Item.FirstLine;

      while CurrentLine/=null loop

         if VisibleLine in

           CurrentVisibleLine..CurrentVisibleLine+CurrentLine.SubLines-1 then

            SubLinePosition:=SelectSubLine
              (Line    => CurrentLine,
               SubLine => VisibleLine-CurrentVisibleLine);
            Line:=CurrentLine;
            return;

         end if;

         CurrentVisibleLine := CurrentVisibleLine+CurrentLine.SubLines;
         CurrentLine        := CurrentLine.Next;

      end loop;

      Line := null;

   end SelectLine;
   ---------------------------------------------------------------------------

   procedure RemoveInvisibleCanvasLines
     (Item : access TextView_Type) is
   begin
      null;
   end;
   pragma Unreferenced(RemoveInvisibleCanvasLines);
   ---------------------------------------------------------------------------

   procedure RenderAndInsertCanvasLine
     (Item               : access TextView_Type;
      PreviousCanvasLine : in out CanvasLine_Access;
      WrappedLine        : Natural;
      Line               : Line_Access;
      SubLinePosition    : Natural) is

      use type Fonts.Font_ClassAccess;

      NewCanvasLine : CanvasLine_Access;

   begin

      if Line.Content(SubLinePosition).LineWidth=0 then
         return;
      end if;

      NewCanvasLine:=new CanvasLine_Type;

      --DEBUG
      if Item.Font=null then
         Put("???????????");
         New_Line;
      end if;

      Item.Context.NewCanvas
        (Object => Object_ClassAccess(Item),
         Height => Item.LineHeight,
         Width  => Line.Content(SubLinePosition).LineWidth,
         Canvas => NewCanvasLine.Canvas);
      NewCanvasLine.Canvas.Clear
        (Color => 16#FFFFFFFF#);
      Item.Font.TextOut
        (Canvas => Canvas.BasicCanvas_ClassAccess(NewCanvasLine.Canvas),
         X      => 0,
         Y      => 0,
         Text   =>
           ToUnboundedString
           (Line.Content
              (SubLinePosition..Line.Content(SubLinePosition).NextLine-1)),
         Color  => 16#FF000000#);

      NewCanvasLine.WrappedLine:=WrappedLine;
      NewCanvasLine.Last:=PreviousCanvasLine;

      GUI.SetBounds
        (Canvas => NewCanvasLine.Canvas,
         Bounds =>
           (Top     => (WrappedLine-Item.FirstWrappedLine)*Item.LineHeight,
            Left    => 0,
            Height  => Item.LineHeight,
            Width   => Line.Content(SubLinePosition).LineWidth,
            Visible => True));

      if PreviousCanvasLine/=null then

         NewCanvasLine.Next      := PreviousCanvasLine.Next;
         if NewCanvasLine.Next/=null then
            NewCanvasLine.Next.Last:=NewCanvasLine;
         end if;

         PreviousCanvasLine.Next := NewCanvasLine;

      else

         Item.CanvasLines := NewCanvasLine;

      end if;

      PreviousCanvasLine:=NewCanvasLine;

   end RenderAndInsertCanvasLine;
   ---------------------------------------------------------------------------

   procedure CompleteCanvasLines
     (Item : access TextView_Type) is

      CurrentWrappedLine       : Natural;
      CurrentSubLinePosition   : Natural;
      LineCursor               : Line_Access;
      CanvasLineCursor         : CanvasLine_Access;
      PreviousCanvasLineCursor : CanvasLine_Access;

   begin

      CurrentWrappedLine:=Item.FirstWrappedLine;

      SelectLine
        (Item            => Item,
         VisibleLine     => CurrentWrappedLine,
         Line            => LineCursor,
         SubLinePosition => CurrentSubLinePosition);

      if LineCursor=null then
         return;
      end if;

      CanvasLineCursor         := Item.CanvasLines;
      PreviousCanvasLineCursor := null;

      -- With this range it may happen one line too much is shown (rare)
      for y in 0..Item.Priv.Bounds.Height/Item.LineHeight loop

         while (CanvasLineCursor/=null)
           and then (CanvasLineCursor.WrappedLine<=CurrentWrappedLine) loop
            PreviousCanvasLineCursor := CanvasLineCursor;
            CanvasLineCursor         := CanvasLineCursor.Next;
         end loop;

         if not
           ((PreviousCanvasLineCursor/=null)
            and then (PreviousCanvasLineCursor.WrappedLine=CurrentWrappedLine)) then

            RenderAndInsertCanvasLine
              (Item               => Item,
               PreviousCanvasLine => PreviousCanvasLineCursor,
               WrappedLine        => CurrentWrappedLine,
               Line               => LineCursor,
               SubLinePosition    => CurrentSubLinePosition);

            CanvasLineCursor:=PreviousCanvasLineCursor.Next;

         end if;

         CurrentSubLinePosition
           :=LineCursor.Content(CurrentSubLinePosition).NextLine;

         if CurrentSubLinePosition>=LineCursor.Content'Last then
            LineCursor:=LineCursor.Next;
            if LineCursor=null then
               return;
            end if;
            CurrentSubLinePosition:=1;
         end if;

         CurrentWrappedLine:=CurrentWrappedLine+1;

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

         Put("Consume Part Word");
         New_Line;
         if LinePosition<=Length(Str) then
            LinePosition:=LinePosition+1;
         end if;

         while (LinePosition<=Length(Str))
           and then (Line.Content(LinePosition).Char/=' ') loop

            Width:=Item.Font.TextWidth
              (Unbounded_Slice(Str,WordStart,LinePosition));
            if LineWidth+Width>Item.Priv.Bounds.Width then
               LineWidth:=LineWidth+Width;
               return;
            end if;
            Put("+");

            LinePosition:=LinePosition+1;

         end loop;

         LineWidth:=LineWidth+Item.Font.TextWidth
              (Unbounded_Slice(Str,WordStart,LinePosition));

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

      Line.SubLines := 0;
      Str           := ToUnboundedString(Line.Content.all);

      while LinePosition<Line.Content'Last loop

         Line.SubLines:=Line.SubLines+1;

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

         Line.Content(LineStart).NextLine  := LinePosition;
         Line.Content(LineStart).LineWidth := LineWidth;

         LineStart := LinePosition;
         LineWidth := 0;

      end loop;

      if Line.SubLines=0 then
         Line.SubLines:=1;
      end if;

   end AddLineBreaks;
   ---------------------------------------------------------------------------

   procedure CalculateLineBreaks
     (Item : access TextView_Type) is

      Line : Line_Access;

   begin
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
         ColorString.Clear(Line.Content);
         Free(Line);
         Line     := NextLine;
      end loop;

      Item.FirstLine:=null;

      ClearCanvasLines(Item);

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

      -- TODO : Replace this by a faster call later!
      CalculateLineBreaks(Item);
      CompleteCanvasLines(Item);
      Put("WriteLine Done");
      New_Line;

   end WriteLine;
   ---------------------------------------------------------------------------

   procedure Resize
     (Item : access TextView_Type) is
   begin

      if Item.Priv.Bounds.Width/=Item.Priv.PrevBounds.Width then
         CalculateLineBreaks(Item);
         ClearCanvasLines(Item);
         CompleteCanvasLines(item);
      end if;

   end Resize;

   procedure SetFont
     (Item : TextView_Access;
      Font : Fonts.Font_ClassAccess) is
   begin
      -- TODO: Calculate Font Specific things.
      Item.Font:=Font;
   end SetFont;
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

      use type Fonts.Font_ClassAccess;

   begin

      Clear(Item);

      if Item.Font/=null then
         Fonts.Release(Item.Font);
      end if;

      GUI.Finalize
        (Item => Object_Access(Item));

   end Finalize;
   ---------------------------------------------------------------------------

end GUI.TextView;
