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

with GUI.TextBasis;
with GUI.ScrollBar;
with GUI.Themes.YellowBlue.VerticalScrollBar;
with Fonts;

--with Ada.Text_IO; use Ada.Text_IO;

package body GUI.Themes.YellowBlue.Console is

   type Console_Type is new GUI.Console.Console_Type with
      record
         TextBasis         : GUI.TextBasis.TextBasis_ClassAccess:=null;
         VerticalScrollBar : GUI.ScrollBar.ScrollBar_ClassAccess:=null;
         EditLineNumber    : Natural := 0;
      end record;
   type Console_Access is access Console_Type;

   overriding
   procedure WriteLine
     (Item   : access Console_Type;
      String : Unbounded_String;
      Color  : Canvas.Color_Type);

   overriding
   procedure SetFont
     (Item : access Console_Type;
      Font : Fonts.Font_ClassAccess);

   procedure SetFont
     (Item   : access Console_Type;
      Font   : Fonts.Font_ClassAccess) is
   begin
      Item.TextBasis.SetFont
        (Font => Font);
   end SetFont;
   ---------------------------------------------------------------------------

   procedure WriteLine
     (Item   : access Console_Type;
      String : Unbounded_String;
      Color  : Canvas.Color_Type) is
   begin
      Item.TextBasis.InsertBefore
        (LineNumber => Item.EditLineNumber,
         String     => String,
         Color      => Color);
      Item.EditLineNumber:=Item.EditLineNumber+1;
   end WriteLine;
   ---------------------------------------------------------------------------

   function NewConsole
     (Parent : Object_ClassAccess)
      return GUI.Console.Console_ClassAccess is

      NewConsole : Console_Access;

   begin

      NewConsole          := new Console_Type;

      GUI.Console.Initialize
        (Item   => GUI.Console.Console_Access(NewConsole),
         Parent => Parent);

      NewConsole.TextBasis := new GUI.TextBasis.TextBasis_Type;

      GUI.TextBasis.Initialize
        (Item   => GUI.TextBasis.TextBasis_Access(NewConsole.TextBasis),
         Parent => Object_ClassAccess(NewConsole));

      NewConsole.TextBasis.EnableInput(0,To_Unbounded_String(">"));

      NewConsole.VerticalScrollBar
        :=GUI.Themes.YellowBlue.VerticalScrollBar.NewVerticalScrollBar
          (Parent => Object_ClassAccess(NewConsole));

      NewConsole.TextBasis.SetBounds
        (Top     => 0,
         Left    => 0,
         Height  => 0,
         Width   => -GUI.Themes.YellowBlue.VerticalScrollBar.VerticalScrollBarWidth,
         Visible => True);
      NewConsole.TextBasis.SetAnchors
        (Top    => True,
         Left   => True,
         Right  => True,
         Bottom => True);
      NewConsole.VerticalScrollBar.SetBounds
        (Top     => 0,
         Left    => -GUI.Themes.YellowBlue.VerticalScrollBar.VerticalScrollBarWidth,
         Height  => 0,
         Width   => GUI.Themes.YellowBlue.VerticalScrollBar.VerticalScrollBarWidth,
         Visible => True);
      NewConsole.VerticalScrollBar.SetAnchors
        (Top    => True,
         Left   => False,
         Right  => True,
         Bottom => True);

      return GUI.Console.Console_ClassAccess(NewConsole);

   end NewConsole;
   ---------------------------------------------------------------------------

end GUI.Themes.YellowBlue.Console;
