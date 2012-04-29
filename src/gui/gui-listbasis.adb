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
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body GUI.ListBasis is

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => ListBasisCanvas_Type,
      Name   => ListBasisCanvas_Access);

   function VisibleLineCount
     (Item : access ListBasis_Type)
      return Integer is

   begin
      return Item.GetBounds.Height/Item.Font.Height;
   end VisibleLineCount;
   ---------------------------------------------------------------------------

   procedure ClearCanvasse
     (Item : access ListBasis_Type) is

      Cursor     : ListBasisCanvas_Access;
      NextCursor : ListBasisCanvas_Access;

   begin

      Cursor:=Item.CanvasLines;
      while Cursor/=null loop
         NextCursor:=Cursor.Next;
         GUI.FreeCanvas(Cursor.Canvas);
         Free(Cursor);
         Cursor:=NextCursor;
      end loop;

   end ClearCanvasse;
   ---------------------------------------------------------------------------

   procedure DrawCanvasse
     (Item : access ListBasis_Type) is

      use type StringandColorList_Pack.Cursor;
      use type Fonts.Font_ClassAccess;

      Entr           : StringAndColorList_Pack.Cursor;
      PreviousCanvas : ListBasisCanvas_Access:=null;
      NewCanvas      : ListBasisCanvas_Access;
      StringAndColor : StringAndColor_Type;
      yPosition      : Integer;
      FontHeight     : Integer;
      TextWidth      : Integer;
      CanvasWidth    : Integer;
      LineNumber     : Integer;
      ObjectWidth    : Integer:=Item.GetBounds.Width;

   begin
      ClearCanvasse(Item);
      if Item.Font=null then
         return;
      end if;
      Entr:=Item.Entries.First;

      for i in 1..Item.TopIndex loop
         if Entr=StringAndColorList_Pack.No_Element then
            return;
         end if;
         Entr:=StringAndColorList_Pack.Next(Entr);
      end loop;

      yPosition:=0;
      FontHeight := Item.Font.Height;

      LineNumber:=Item.TopIndex;

      while Entr/=StringAndColorList_Pack.No_Element loop

         Put("Line");
         Put(LineNumber);
         Put(Item.SelectedIndex);
         New_Line;
         StringAndColor:=StringAndColorList_Pack.Element(Entr);

         NewCanvas:=new ListBasisCanvas_Type;
         TextWidth:=Item.Font.TextWidth(StringAndColor.String);

         if LineNumber/=Item.SelectedIndex then
            CanvasWidth:=Integer'Min(TextWidth,ObjectWidth);
         else
            CanvasWidth := ObjectWidth;
         end if;
         Put(ObjectWidth);
         Put(CanvasWidth);
         Put(TextWidth);
         New_Line;

         NewCanvas.Canvas:=Item.NewCanvas
           (Height => FontHeight,
            Width  => CanvasWidth);

         if LineNumber/=Item.SelectedIndex then
            NewCanvas.Canvas.Clear(16#FF00FF00#);
         else
            NewCanvas.Canvas.Clear(16#FFFF0000#);
         end if;

         Item.Font.TextOut
           (Canvas => Canvas.Canvas_ClassAccess(NewCanvas.Canvas),
            X      => 0.0,
            Y      => 0.0,
            Text   => StringAndColor.String,
            Color  => StringAndColor.Color);
         NewCanvas.Canvas.SetBounds
           (Top     => yPosition,
            Left    => 0,
            Height  => FontHeight,
            Width   => TextWidth,
            Visible => True);
         Put(NewCanvas.Canvas.GetBounds);
         Put("ContentWidth");
         Put(NewCanvas.Canvas.ContentWidth);
         New_Line;

         NewCanvas.Last:=PreviousCanvas;
         if PreviousCanvas/=null then
            PreviousCanvas.Next:=NewCanvas;
         else
            Item.CanvasLines:=NewCanvas;
         end if;

         yPosition:=yPosition+FontHeight;
         if YPosition>=Item.GetBounds.Height then
            exit;
         end if;

         Entr       := StringAndColorList_Pack.Next(Entr);
         LineNumber := LineNumber+1;

      end loop;

   end DrawCanvasse;
   ---------------------------------------------------------------------------

   procedure Finalize
     (Item : access ListBasis_Type) is
   begin
      GUI.Object_Access(Item).Finalize;
   end Finalize;
   ---------------------------------------------------------------------------

   procedure Resize
     (Item : access ListBasis_Type) is
   begin
      DrawCanvasse(Item);
   end Resize;
   ---------------------------------------------------------------------------

   procedure SetFont
     (Item : access ListBasis_Type;
      Font : Fonts.Font_ClassAccess) is

      use type Fonts.Font_ClassAccess;

   begin
      if Item.Font/=null then
         Fonts.Release(Item.Font);
      end if;
      Item.Font:=Font;
      DrawCanvasse(Item);
   end SetFont;
   ---------------------------------------------------------------------------

   procedure AddEntry
     (Item   : access ListBasis_Type;
      String : Unbounded_String;
      Color  : Canvas.Color_Type) is
   begin

      Item.Entries.Append((String,Color));
      DrawCanvasse(Item);

   end AddEntry;
   ---------------------------------------------------------------------------

   procedure ClearEntries
     (Item : access ListBasis_Type) is
   begin

      Item.Entries.Clear;
      DrawCanvasse(Item);

   end ClearEntries;
   ---------------------------------------------------------------------------

end GUI.ListBasis;
