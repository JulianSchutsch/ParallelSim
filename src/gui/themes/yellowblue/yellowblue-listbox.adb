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

with YellowBlue.VerticalScrollBar;
with GUI.ListBasis;
with GUI.ScrollBar;
with Canvas;
with Fonts;
with Basics; use Basics;
with BoundsCalc; use BoundsCalc;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body YellowBlue.ListBox is

   type ListBox_Type is new GUI.ListBox.ListBox_Type with
      record
         ListBasis         : GUI.ListBasis.ListBasis_Access      := null;
         VerticalScrollbar : GUI.ScrollBar.ScrollBar_ClassAccess := null;
         EntryCount        : Integer:=0;
      end record;
   type ListBox_Access is access ListBox_Type;

   overriding
   procedure AddEntry
     (Item   : access ListBox_Type;
      String : Unbounded_String;
      Color  : Canvas.Color_Type);

   overriding
   procedure ClearEntries
     (Item : access ListBox_Type);
   ---------------------------------------------------------------------------

   procedure UpdateScrollBar
     (Item : access ListBox_Type) is

      ReqScroll : Integer;
      Position  : Integer;

   begin
      ReqScroll:=Item.EntryCount-Item.ListBasis.VisibleLineCount;
      if ReqScroll<0 then
         ReqScroll:=0;
      end if;

      Position:=Item.VerticalScrollbar.GetPosition;
      if Position>ReqScroll then
         Position:=ReqScroll;
      end if;

      Put("ReqScroll");
      Put(ReqScroll);

      Item.VerticalScrollbar.SetRange
        (Min      => 0,
         Max      => ReqScroll,
         Position => Position);

   end UpdateScrollBar;
   ---------------------------------------------------------------------------

   procedure ClearEntries
     (Item : access ListBox_Type) is
   begin
      Item.ListBasis.ClearEntries;
   end ClearEntries;
   ---------------------------------------------------------------------------

   procedure AddEntry
     (Item   : access ListBox_Type;
      String : Unbounded_String;
      Color  : Canvas.Color_Type) is
   begin
      Item.ListBasis.AddEntry(String,Color);
      Item.EntryCount:=Item.EntryCount+1;
      UpdateScrollBar(Item);
   end AddEntry;
   ---------------------------------------------------------------------------


   function NewListBox
     (Parent : GUI.Object_ClassAccess)
      return GUI.ListBox.ListBox_ClassAccess is

      NewListBox : ListBox_Access;
      Bounds     : Bounds_Type;

   begin
      NewListBox:=new ListBox_Type;
      GUI.ListBox.ListBox_Access(NewListBox).Initialize(Parent);
      Bounds:=NewListBox.GetBounds;

      NewListBox.ListBasis
        :=new GUI.ListBasis.ListBasis_Type;

      GUI.ListBasis.ListBasis_Access(NewListBox.ListBasis)
        .Initialize(GUI.Object_ClassAccess(NewListBox));
      NewListBox.ListBasis.SetBounds
        (Top     => 0,
         Left    => 0,
         Height  => Bounds.Height,
         Width   => Bounds.Width-YellowBlue.VerticalScrollBar.VerticalScrollbarWidth,
         Visible => True);
      NewListBox.ListBasis.SetAnchors
        (Top    => True,
         Left   => True,
         Right  => True,
         Bottom => True);
      NewListBox.ListBasis.SetFont
        (Fonts.Lookup
        (Name       => U("Vera"),
         Size       => 25,
         Attributes => Fonts.NoAttributes));

      NewListBox.VerticalScrollbar
        :=YellowBlue.VerticalScrollBar.NewVerticalScrollBar
          (GUI.Object_ClassAccess(NewListBox));
      NewListBox.VerticalScrollbar.SetRange
        (Min      => 0,
         Max      => 0,
         Position => 0);
      NewListBox.VerticalScrollbar.SetBounds
        (Top     => 0,
         Left    => Bounds.Width-YellowBlue.VerticalScrollBar.VerticalScrollbarWidth,
         Height  => Bounds.Height,
         Width   => YellowBlue.VerticalScrollBar.VerticalScrollbarWidth,
         Visible => True);
      NewListBox.VerticalScrollbar.SetAnchors
        (Top    => True,
         Left   => False,
         Right  => True,
         Bottom => True);

      return GUI.ListBox.ListBox_ClassAccess(NewListBox);

   end NewListBox;
   ---------------------------------------------------------------------------

end YellowBlue.ListBox;
