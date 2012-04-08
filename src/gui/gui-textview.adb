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
with Canvas;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body GUI.TextView is

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Line_Type,
      Name   => Line_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => CanvasLine_Type,
      Name   => CanvasLine_Access);

   function MouseDown
     (Item   : access TextView_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer)
      return Boolean is

      pragma Unreferenced(Item);
      pragma Unreferenced(X);
      pragma Unreferenced(Y);
      pragma Unreferenced(Button);

   begin
      return True;
   end MouseDown;
   ---------------------------------------------------------------------------

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

   procedure SelectLine
     (Item            : access TextView_Type;
      VisibleLine     : Natural;
      Line            : out Line_Access) is

      CurrentLine        : Line_Access;
      CurrentVisibleLine : Integer:=0;

   begin

      CurrentLine:=Item.FirstLine;

      while CurrentLine/=null loop

         if VisibleLine in

           CurrentVisibleLine..CurrentVisibleLine+CurrentLine.WrappedLineCount-1 then

            Put("SelectLine:");
            Put(VisibleLine-CurrentVisibleLine);
            CurrentLine.SelectWrappedLine(VisibleLine-CurrentVisibleLine);
            Line:=CurrentLine;
            return;

         end if;

         CurrentVisibleLine := CurrentVisibleLine+CurrentLine.WrappedLineCount;
         CurrentLine        := CurrentLine.Next;

      end loop;

      Line := null;

   end SelectLine;
   ---------------------------------------------------------------------------

   procedure RenderCanvasLine
     (Item       : access TextView_Type;
      CanvasLine : CanvasLine_Access) is

      use type Fonts.Font_ClassAccess;

   begin
      if Item.Font=null then
         return;
      end if;

      Item.Context.NewCanvas
        (Object => Object_ClassAccess(Item),
         Height => Item.LineHeight,
         Width  => CanvasLine.Line.CurrentWidth,
         Canvas => CanvasLine.Canvas);

      CanvasLine.Canvas.Clear
        (Color => 0);-- 16#FFFFFFFF#);

      CanvasLine.Line.Render
        (Canvas => Canvas.Canvas_ClassAccess(CanvasLine.Canvas),
         X      => 0,
         Y      => 0);

      CanvasLine.Canvas.SetBounds
        (Top     => (CanvasLine.WrappedLine-Item.FirstWrappedLine)*Item.LineHeight,
         Left    => 0,
         Height  => Item.LineHeight,
         Width   => CanvasLine.Line.CurrentWidth,
         Visible => True);

   end;
   ---------------------------------------------------------------------------

   function InsertCanvasLine
     (Item         : access TextView_Type;
      CanvasLine   : CanvasLine_Access;
      WrappedLine  : Natural;
      Line         : Line_Access)
      return CanvasLine_Access is

      NewCanvasLine : CanvasLine_Access;

   begin

      if Line.CurrentWidth<=0 then
         return CanvasLine;
      end if;

      NewCanvasLine:=new CanvasLine_Type;

      NewCanvasLine.WrappedLine := WrappedLine;
      NewCanvasLine.Last        := CanvasLine;
      NewCanvasLine.Line        := Line;

      if CanvasLine/=null then

         NewCanvasLine.Next      := CanvasLine.Next;
         if NewCanvasLine.Next/=null then
            NewCanvasLine.Next.Last:=NewCanvasLine;
         end if;

         CanvasLine.Next := NewCanvasLine;

      else

         Item.CanvasLines := NewCanvasLine;

      end if;

      return NewCanvasLine;

   end InsertCanvasLine;
   ---------------------------------------------------------------------------

   procedure CompleteCanvasLines
     (Item : access TextView_Type) is

      CurrentWrappedLine : Natural;
      Line               : Line_Access;
      CanvasLine         : CanvasLine_Access;

      function FindCanvasLine
        return Boolean is

         Cursor : CanvasLine_Access;

      begin

         Cursor:=CanvasLine;
         while Cursor/=null
           and then Cursor.WrappedLine<=CurrentWrappedLine loop
            CanvasLine:=Cursor;
            Cursor:=Cursor.Next;
         end loop;

         return (CanvasLine/=null)
           and then (CanvasLine.WrappedLine=CurrentWrappedLine);

      end FindCanvasLine;
      ------------------------------------------------------------------------

   begin

      CurrentWrappedLine:=Item.FirstWrappedLine;

      SelectLine
        (Item            => Item,
         VisibleLine     => CurrentWrappedLine,
         Line            => Line);

      if Line=null then
         return;
      end if;

      WrappedLineLoop1:
      loop

         CanvasLine:=InsertCanvasLine
           (Item         => Item,
            CanvasLine   => CanvasLine,
            WrappedLine  => CurrentWrappedLine,
            Line         => Line);

         if CanvasLine/=null then
            RenderCanvasLine
              (Item => Item,
               CanvasLine => CanvasLine);
         end if;

         CurrentWrappedLine:=CurrentWrappedLine+1;

         if (CurrentWrappedLine-Item.FirstWrappedLine)*Item.LineHeight>Item.Bounds.Height then
            return;
         end if;
         exit WrappedLineLoop1 when not Line.NextWrappedLine;

      end loop WrappedLineLoop1;

      Line:=Line.Next;

      CanvasLine := Item.CanvasLines;

      LogicalLineLoop:
      while Line/=null loop

         if Line.FirstWrappedLine then

            WrappedLineLoop:
            loop

               if not FindCanvasLine then

                  CanvasLine:=InsertCanvasLine
                    (Item         => Item,
                     CanvasLine   => CanvasLine,
                     WrappedLine  => CurrentWrappedLine,
                     Line         => Line);

                  if CanvasLine/=null then
                     RenderCanvasLine
                       (Item => Item,
                        CanvasLine => CanvasLine);
                  end if;

               end if;

               CurrentWrappedLine:=CurrentWrappedLine+1;

               exit LogicalLineLoop when (CurrentWrappedLine-Item.FirstWrappedLine)*Item.LineHeight>Item.GetBounds.Height;
               exit WrappedLineLoop when not Line.NextWrappedLine;

            end loop WrappedLineLoop;

         end if;

         Line:=Line.Next;

      end loop LogicalLineLoop;

   end CompleteCanvasLines;
   ---------------------------------------------------------------------------

   procedure CalculateLineBreaks
     (Item : access TextView_Type) is

      Line : Line_Access;

   begin
      Line:=Item.FirstLine;
      while Line/=null loop
         Line.GreedyWrapping
           (Width => Item.Bounds.Width);
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
         Line.Clear;
         Free(Line);
         Line     := NextLine;
      end loop;

      Item.FirstLine:=null;

      ClearCanvasLines(Item);

   end Clear;
   ---------------------------------------------------------------------------

   function GetFirstWrappedLine
     (TextView : access TextView_Type;
      Line     : Line_Access)
      return Integer is

      pragma Unreferenced(TextView);

      Cursor       : Line_Access:=Line.Last;
      WrappedLines : Integer:=0;

   begin

      while Cursor/=null loop
         WrappedLines:=WrappedLines+Cursor.WrappedLineCount;
         Cursor:=Cursor.Last;
      end loop;

      if WrappedLines=0 then
         return 0;
      else
         return WrappedLines-1;
      end if;

   end GetFirstWrappedLine;
   ---------------------------------------------------------------------------

   procedure SetFirstWrappedLine
     (TextView   : access TextView_Type;
      LineNumber : Integer) is
   begin
      --TODO: Make it faster (different call)
      Put("Goto");
      Put(LineNumber);
      New_Line;
      TextView.FirstWrappedLine:=LineNumber;

      ClearCanvasLines(TextView);
      CompleteCanvasLines(TextView);

   end SetFirstWrappedLine;
   ---------------------------------------------------------------------------

   procedure MakeVisible
     (TextView : access TextView_Type;
      Line     : Line_Access;
      Position : Integer) is

      WrappedLine  : Integer;
      VisibleLines : constant Integer
        :=TextView.GetBounds.Height/TextView.LineHeight-1;

   begin
      Put("Make Visible");
      Put(Position);
      Put(GetFirstWrappedLine(TextView,Line));
      New_Line;
      WrappedLine:=Line.GetWrappedLine(Position)
        +GetFirstWrappedLine(TextView,Line);
      Put(WrappedLine);
      Put(TextView.FirstWrappedLine);
      Put(VisibleLines);
      New_Line;

      if WrappedLine<TextView.FirstWrappedLine then
         Put("ScrollUp");
         New_Line;
         SetFirstWrappedLine(TextView,WrappedLine);
         return;
      end if;

      if WrappedLine-TextView.FirstWrappedLine>VisibleLines then
         Put("ScrollDown");
         New_Line;
         SetFirstWrappedLine(TextView,WrappedLine-VisibleLines);
      end if;

   end MakeVisible;

   function InsertCharacters
     (Item     : access TextView_Type;
      Line     : Line_Access;
      Position : Natural;
      String   : Unbounded_String;
      Color    : Canvas.Color_Type)
      return Integer is

      Result : Integer;
   begin
      Result:=Line.Insert
        (Position => Position,
         String   => String,
         Color    => Color);
      Line.GreedyWrapping(Item.Bounds.Width);
      ClearCanvasLines(Item); -- TEMP HACK
      CompleteCanvasLines(Item);

      return Result;
   end InsertCharacters;
   ---------------------------------------------------------------------------

   -- TODO: Visible Lines change easily with this, update them!
   -- TEMPFIX : Clearing all
   procedure InsertBefore
     (Item   : access TextView_Type;
      Line   : Line_Access;
      String : Unbounded_String;
      Color  : Canvas.Color_Type) is

      NewLine : Line_Access;

   begin
      NewLine:=new Line_Type;
      NewLine.Initialize
        (String      => String,
         Color       => Color,
         Font        => Item.Font);

      NewLine.Next:=Line;
      NewLine.Last:=Line.Last;
      Line.Last:=NewLine;
      if NewLine.Last/=null then
         NewLine.Last.Next:=NewLine;
      else
         Item.FirstLine:=NewLine;
      end if;

      NewLine.GreedyWrapping(Item.Bounds.Width);
      ClearCanvasLines(Item); -- TEMP HACK
      CompleteCanvasLines(Item);

   end InsertBefore;
   ---------------------------------------------------------------------------

   function NewLine
     (Item : access TextView_Type;
      String : Unbounded_String;
      Color : Canvas.Color_Type)
      return Line_Access is

      NewLine : Line_Access;

   begin

      NewLine:=new Line_Type;
      NewLine.Initialize
        (String      => String,
         Color       => Color,
         Font        => Item.Font);

      NewLine.Last:=Item.LastLine;
      if Item.LastLine/=null then
         Item.LastLine.Next:=NewLine;
      else
         Item.FirstLine:=NewLine;
      end if;
      Item.LastLine:=NewLine;

      NewLine.GreedyWrapping(Item.Bounds.Width);
      CompleteCanvasLines(Item);

      return NewLine;

   end NewLine;
   ---------------------------------------------------------------------------

   procedure WriteLine
     (Item   : access TextView_Type;
      String : Unbounded_String;
      Color  : Canvas.Color_Type) is

      NewLine : Line_Access;
   begin

      NewLine:=new Line_Type;
      NewLine.Initialize
        (String      => String,
         Color       => Color,
         Font        => Item.Font);

      NewLine.Last:=Item.LastLine;
      if Item.LastLine/=null then
         Item.LastLine.Next:=NewLine;
      else
         Item.FirstLine:=NewLine;
      end if;
      Item.LastLine:=NewLine;

      NewLine.GreedyWrapping(Item.Bounds.Width);
      CompleteCanvasLines(Item);

   end WriteLine;
   ---------------------------------------------------------------------------

   procedure Resize
     (Item : access TextView_Type) is
   begin

      if (Item.Bounds.Height=Item.PrevBounds.Height)
        and (Item.Bounds.Width=Item.PrevBounds.Width) then
         return;
      end if;

      ClearCanvasLines(Item);
      if Item.Bounds.Width/=Item.PrevBounds.Width then
         CalculateLineBreaks(Item);
      end if;

      CompleteCanvasLines(Item);

   end Resize;
   ---------------------------------------------------------------------------

   procedure Reinitialize
     (Item : access TextView_Type) is

      Line : Line_Access;

   begin

      Line:=Item.FirstLine;
      while Line/=null loop
         Line.Reinitialize(Item.Font);
         Line:=Line.Next;
      end loop;

   end Reinitialize;
   ---------------------------------------------------------------------------

   procedure SetFont
     (Item : access TextView_Type;
      Font : Fonts.Font_ClassAccess) is
   begin
      -- TODO: Calculate Font Specific things.
      Item.Font           := Font;
      Item.SpaceCharWidth := Font.TextWidth(To_Unbounded_String(" "));
      Item.LineHeight     := Font.Height;

      Reinitialize(Item);
      ClearCanvasLines(Item);
      CalculateLineBreaks(Item);
      CompleteCanvasLines(Item);

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
