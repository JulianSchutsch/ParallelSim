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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Fonts;
with Basics; use Basics;
with BoundsCalc; use BoundsCalc;
with Canvas;

package body YellowBlue.Button is

   type Button_Type is new GUI.Button.Button_Type with
      record
         Font   : Fonts.Font_ClassAccess;
         Canvas : Canvas_ClassAccess;
      end record;
   type Button_Access is access Button_Type;

   overriding
   procedure SetCaption
     (Item    : access Button_Type;
      Caption : Unbounded_String);

   overriding
   procedure Resize
     (Item : access Button_Type);
   ---------------------------------------------------------------------------

   procedure DrawCanvas
     (Item : access Button_Type) is

      use type Fonts.Font_ClassAccess;

      Bounds : Bounds_Type:=Item.GetBounds;

   begin

      if Item.Canvas/=null then
         Item.Canvas.Finalize;
      end if;
      if (Bounds.Height<=0)
        or (Bounds.Width<=0) then
         return;
      end if;

      Item.Canvas:=Item.NewCanvas
        (Height => Bounds.Height,
         Width  => Bounds.Width);

      Item.Canvas.Clear(16#FF7F0000#);
      Item.Canvas.Rectangle
        (X => 0,
         Y => 0,
         Height => Bounds.Height,
         Width => Bounds.Width,
         Color => 16#FFFFFFFF#);

      Item.Canvas.SetBounds
        (Top     => 0,
         Left    => 0,
         Height  => Bounds.Height,
         Width   => Bounds.Width,
         Visible => True);

      if Item.Font/=null then

         declare
            Width  : Integer:=Item.Font.TextWidth(Item.Caption);
            Height : Integer:=Item.Font.Height;
         begin
            Item.Font.TextOut
              (Canvas => Canvas.Canvas_ClassAccess(Item.Canvas),
               X => Float((Bounds.Width-Width)/2-1),
               Y => Float((Bounds.Height-Height)/2-1),
               Text => Item.Caption,
               Color => 16#FFFFFFFF#);
         end;

      end if;

   end DrawCanvas;
   ---------------------------------------------------------------------------

   procedure SetCaption
     (Item    : access Button_Type;
      Caption : Unbounded_String) is
   begin
      GUI.Button.Button_Access(Item).SetCaption(Caption);
      DrawCanvas(Item);
   end SetCaption;
   ---------------------------------------------------------------------------

   procedure Resize
     (Item : access Button_Type) is
   begin
      DrawCanvas(Item);
   end Resize;
   ---------------------------------------------------------------------------

   function NewButton
     (Parent : Object_ClassAccess)
      return Button_ClassAccess is

      NewButton : Button_Access;

   begin
      NewButton:=new Button_Type;
      Object_Access(NewButton).Initialize(Parent);
      NewButton.Font:=Fonts.Lookup
        (Name       => U("./Vera.ttf"),
         Size       => 25,
         Attributes => Fonts.NoAttributes);
      return Button_ClassAccess(NewButton);
   end NewButton;
   ---------------------------------------------------------------------------

end YellowBlue.Button;
