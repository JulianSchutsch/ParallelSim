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
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
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

      if Line.Content(SubLinePosition).LineWidth<=0 then
         return;
      end if;

      NewCanvasLine:=new CanvasLine_Type;

      if Item.Font=null then
         raise NoFontSelected;
      end if;

      Item.Context.NewCanvas
        (Object => Object_ClassAccess(Item),
         Height => Item.LineHeight,
         Width  => Line.Content(SubLinePosition).LineWidth,
         Canvas => NewCanvasLine.Canvas);

      NewCanvasLine.Canvas.Clear
        (Color => 16#FFFFFFFF#);
      declare
         X : Integer:=0;
         Y : Integer:=0;
      begin
         for i in SubLinePosition..Line.Content(SubLinePosition).NextLine-1 loop
            Item.Font.CharacterOut
              (Canvas => Canvas.Canvas_ClassAccess(NewCanvasLine.Canvas),
               X      => X,
               Y      => Y,
               Char   => Line.Content(i).Char,
               Color  => Line.Content(i).Color);
         end loop;
      end;

      NewCanvasLine.WrappedLine:=WrappedLine;
      NewCanvasLine.Last:=PreviousCanvasLine;

      NewCanvasLine.Canvas.SetBounds
        (Top     => (WrappedLine-Item.FirstWrappedLine)*Item.LineHeight,
         Left    => 0,
         Height  => Item.LineHeight,
         Width   => Line.Content(SubLinePosition).LineWidth,
         Visible => True);

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
              and then
                (PreviousCanvasLineCursor.WrappedLine=CurrentWrappedLine)
              )then

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
   end CompleteCanvasLines;
   ---------------------------------------------------------------------------

   procedure AddLineBreaks
     (Item : access TextView_Type;
      Line : Line_Access) is

      InWord              : Boolean:=False;
      WordStart           : Integer:=0;
      LineAccumWidth      : Integer:=0;
      LineStart           : Natural:=Line.Content'First;
      LineNext            : Natural;
      LineWordCount       : Integer:=0;
      Position            : Natural:=Line.Content'First;
      Overflow            : Boolean;

      procedure StopLine is
      begin

         if LineStart in Line.Content'Range then
            if Line.Content(LineStart).NextLine/=Position then
               Line.Content(LineStart).Modified  := True;
               Line.Content(LineStart).NextLine  := Position;
            end if;
            Line.Content(LineStart).LineWidth := Line.Content(Position-1).AccumWidth-LineAccumWidth;
         end if;

         if Line.SubLines=0 then
            Line.SubLines:=1;
         end if;

      end StopLine;
      ------------------------------------------------------------------------

      procedure NewLine is
      begin

         Line.SubLines:=Line.SubLines+1;
         if Line.Content(LineStart).NextLine/=Position then
            Line.Content(LineStart).Modified  := True;
            Line.Content(LineStart).NextLine  := Position;
         end if;
         Line.Content(LineStart).LineWidth := Line.Content(Position).AccumWidth-LineAccumWidth;
         LineStart      := Position;
         LineAccumWidth := Line.Content(Position).AccumWidth;
         LineWordCount  := 0;
         InWord:=False;

      end NewLine;
      ------------------------------------------------------------------------

   begin

      Line.SubLines:=0;

      while Position<=Line.Content'Last loop

         Overflow:=(Line.Content(Position).AccumWidth-LineAccumWidth>Item.Priv.Bounds.Width);

         if not InWord then

            if Line.Content(Position).Char=' ' then

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

            if Line.Content(Position).Char=' ' then

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
      Position:=Line.Content'First+1;
      LineNext:=Line.Content(Line.Content'First).NextLine;

      while Position<=Line.Content'Last loop
         if Position/=LineNext then
            Line.Content(Position).NextLine:=0;
         else
            LineNext:=Line.Content(Position).NextLine;
         end if;
         Position:=Position+1;
      end loop;

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
         Fonts.ColorStrings.Clear(Line.Content);
         Free(Line);
         Line     := NextLine;
      end loop;

      Item.FirstLine:=null;

      ClearCanvasLines(Item);

   end Clear;
   ---------------------------------------------------------------------------

   procedure CalculateColorStringDimensions
     (Item   : access TextView_Type;
      String : Fonts.ColorStrings.ColorString_Access) is

      AccumWidth   : Integer:=0;
      PreviousChar : Wide_Wide_Character:=Wide_Wide_Character'Val(0);
      ThisChar     : Wide_Wide_Character;

   begin

      for i in String'Range loop

         ThisChar := String(i).Char;
         AccumWidth           := AccumWidth+Item.Font.CharacterWidth(ThisChar);
         String(i).AccumWidth := AccumWidth;

         AccumWidth := AccumWidth+Item.Font.Kerning(PreviousChar,ThisChar);

         PreviousChar:=ThisChar;

      end loop;

   end CalculateColorStringDimensions;
   ---------------------------------------------------------------------------

   WriteLineCount : Integer:=0;

   procedure WriteLine
     (Item   : access TextView_Type;
      String : Unbounded_String;
      Color  : Canvas.Color_Type) is

      NewLine : Line_Access;
   begin

      NewLine:=new Line_Type;
      Fonts.ColorStrings.Append
        (ColorString => NewLine.Content,
         String      => String,
         Color       => Color);
      CalculateColorStringDimensions
        (Item   => Item,
         String => NewLine.Content);

      NewLine.Last:=Item.LastLine;
      if Item.LastLine/=null then
         Item.LastLine.Next:=NewLine;
      else
         Item.FirstLine:=NewLine;
      end if;
      Item.LastLine:=NewLine;

      -- TODO : Replace this by a faster call later!
      AddLineBreaks(Item,NewLine);
      CompleteCanvasLines(Item);
      Put("WriteLine Done");
      Put(WriteLineCount);
      WriteLineCount:=WriteLineCount+1;
      New_Line;

   end WriteLine;
   ---------------------------------------------------------------------------

   procedure Resize
     (Item : access TextView_Type) is
   begin

      ClearCanvasLines(Item);
      if Item.Priv.Bounds.Width/=Item.Priv.PrevBounds.Width then
         CalculateLineBreaks(Item);
      end if;

      RemoveInvisibleCanvasLines(Item);
      CompleteCanvasLines(Item);

   end Resize;

   procedure SetFont
     (Item : TextView_Access;
      Font : Fonts.Font_ClassAccess) is
   begin
      -- TODO: Calculate Font Specific things.
      Item.Font:=Font;
      Item.SpaceCharWidth:=Font.TextWidth(To_Unbounded_String(" "));
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
