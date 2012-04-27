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

with BoundsCalc; use BoundsCalc;
with Canvas;
with Fonts;
with Basics; use Basics;
--with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body YellowBlue.Combobox is

   BackgroundColor  : constant Canvas.Color_Type:=16#FF00007F#;
   FrameColor       : constant Canvas.Color_Type:=16#FFFFFF00#;
   TriangleColor    : constant Canvas.Color_Type:=16#FFFFFFFF#;
   DownButtonWidth  : constant:=25;
   TriangleDistance : constant:=4;

   type Combobox_Type is new GUI.Combobox.Combobox_Type with
      record
         Canvas : GUI.Canvas_ClassAccess:=null;
         Font   : Fonts.Font_ClassAccess:=null;
      end record;
   type Combobox_Access is access Combobox_Type;

   overriding
   procedure Resize
     (Item : access Combobox_Type);

   overriding
   procedure SetIndex
     (Item  : access Combobox_Type;
      Index : Integer);

   overriding
   procedure Finalize
     (Item : access Combobox_Type);
   ---------------------------------------------------------------------------

   procedure Finalize
     (Item : access Combobox_Type) is

      use type Fonts.Font_ClassAccess;

   begin
      if Item.Font/=null then
         Fonts.Release(Item.Font);
      end if;

      GUI.Combobox.Combobox_Access(Item).Finalize;

   end Finalize;
   ---------------------------------------------------------------------------

   procedure DrawCanvas
     (Combobox : access Combobox_Type) is

      use type Fonts.Font_ClassAccess;

      Bounds : Bounds_Type:=Combobox.GetBounds;

   begin

      GUI.FreeCanvas(Combobox.Canvas);
      Combobox.Canvas:=Combobox.NewCanvas
        (Height => Bounds.Height,
         Width  => Bounds.Width);
      Combobox.Canvas.Clear(BackgroundColor);
      Combobox.Canvas.Rectangle
        (X      => 0,
         Y      => 0,
         Height => Bounds.Height,
         Width  => Bounds.Width,
         Color  => FrameColor);
      Combobox.Canvas.VertLine
        (X      => Bounds.Width-DownButtonWidth,
         Y      => 1,
         Height => Bounds.Height-2,
         Color  => FrameColor);
      Combobox.Canvas.WuLine
        (X1    => float(Bounds.Width-DownButtonWidth+TriangleDistance),
         Y1    => float(TriangleDistance),
         X2    => float(Bounds.Width)-float(DownButtonWidth)/2.0-1.0,
         Y2    => float(Bounds.Height-TriangleDistance-1),
         Color => TriangleColor);
      Combobox.Canvas.WuLine
        (X1    => float(Bounds.Width-TriangleDistance-1),
         Y1    => float(TriangleDistance),
         X2    => float(Bounds.Width)-float(DownButtonWidth)/2.0-1.0,
         Y2    => float(Bounds.Height-TriangleDistance-1),
         Color => TriangleColor);
      Combobox.Canvas.WuLine
        (X1    => float(Bounds.Width-DownButtonWidth+TriangleDistance),
         Y1    => float(TriangleDistance),
         X2    => float(Bounds.Width-TriangleDistance-1),
         Y2    => float(TriangleDistance),
         Color => TriangleColor);

      Combobox.Canvas.SetBounds
        (Top     => 0,
         Left    => 0,
         Height  => Bounds.Height,
         Width   => Bounds.Width,
         Visible => True);

      if Combobox.Font/=null then
         declare
            String : Unbounded_String;
            Color  : Canvas.Color_Type;
         begin
            Combobox.GetSelectedEntry(String,Color);
            Combobox.Font.TextOut
              (Canvas => Canvas.Canvas_ClassAccess(Combobox.Canvas),
               X      => 4.0,
               Y      => 2.0,
               Text   => String,
               Color  => Color);
         end;
      end if;

   end DrawCanvas;
   ---------------------------------------------------------------------------

   procedure SetIndex
     (Item  : access Combobox_Type;
      Index : Integer) is
   begin
      GUI.Combobox.Combobox_Access(Item).SetIndex(Index);
      DrawCanvas(Item);
   end SetIndex;
   ---------------------------------------------------------------------------

   procedure Resize
     (Item : access Combobox_Type) is
   begin
      DrawCanvas(Item);
   end Resize;
   ---------------------------------------------------------------------------

   function NewCombobox
     (Parent : GUI.Object_ClassAccess)
      return GUI.Combobox.Combobox_ClassAccess is

      NewCombobox : Combobox_Access;

   begin

      NewCombobox:=new Combobox_Type;
      NewCombobox.Initialize(Parent);

      NewCombobox.Font:=Fonts.Lookup
        (Name       => U("./Vera.ttf"),
         Size       => 16,
         Attributes => Fonts.NoAttributes);

      return Combobox_ClassAccess(NewCombobox);

   end NewCombobox;

end YellowBlue.Combobox;
