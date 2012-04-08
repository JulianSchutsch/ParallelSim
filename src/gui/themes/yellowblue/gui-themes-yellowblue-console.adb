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

with GUI.TextView;
with GUI.ScrollBar;
with GUI.Themes.YellowBlue.VerticalScrollBar;
with Fonts;

with Ada.Text_IO; use Ada.Text_IO;

package body GUI.Themes.YellowBlue.Console is

   type Console_Type is new GUI.Console.Console_Type with
      record
         TextView          : GUI.TextView.TextView_ClassAccess:=null;
         VerticalScrollBar : GUI.ScrollBar.ScrollBar_ClassAccess:=null;
         InputPosition     : Integer;
         PromptLength      : Integer;
         EditLine          : GUI.TextView.Line_Access:=null;
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

   overriding
   procedure Msg_CharacterInput
     (Item  : access Console_Type;
      Chars : Unbounded_String);
   ---------------------------------------------------------------------------

   procedure Msg_CharacterInput
     (Item  : access Console_Type;
      Chars : Unbounded_String) is

   begin

      Put("Receive CHARS:");
      Put(To_String(Chars));
      New_Line;
      Item.InputPosition:=Item.InputPosition
        +Item.TextView.InsertCharacters
        (Line => Item.EditLine,
         Position =>Item.InputPosition,
         String => Chars,
         Color => 16#FFFF00FF#);
      Item.TextView.MakeVisible
        (Line     => Item.EditLine,
         Position => Item.InputPosition);

   end Msg_CharacterInput;
   ---------------------------------------------------------------------------

   procedure SetFont
     (Item   : access Console_Type;
      Font   : Fonts.Font_ClassAccess) is
   begin
      Item.TextView.SetFont
        (Font => Font);
   end SetFont;
   ---------------------------------------------------------------------------

   procedure WriteLine
     (Item   : access Console_Type;
      String : Unbounded_String;
      Color  : Canvas.Color_Type) is
   begin
      Item.TextView.InsertBefore
        (Line => Item.EditLine,
         String => String,
         Color  => Color);
      Item.TextView.MakeVisible
        (Line     => Item.EditLine,
         Position => Item.InputPosition);
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

      NewConsole.TextView := new GUI.TextView.TextView_Type;

      GUI.TextView.Initialize
        (Item   => GUI.TextView.TextView_Access(NewConsole.TextView),
         Parent => Object_ClassAccess(NewConsole));

      NewConsole.EditLine:=NewConsole.TextView.NewLine
        (String => To_Unbounded_String(">"),
         Color  => 16#FFFFFFFF#);

      NewConsole.PromptLength  := 1;
      NewConsole.InputPosition := 2; -- Directly after the prompt

      NewConsole.VerticalScrollBar
        :=GUI.Themes.YellowBlue.VerticalScrollBar.NewVerticalScrollBar
          (Parent => Object_ClassAccess(NewConsole));

      NewConsole.TextView.SetBounds
        (Top     => 0,
         Left    => 0,
         Height  => 0,
         Width   => -GUI.Themes.YellowBlue.VerticalScrollBar.VerticalScrollBarWidth,
         Visible => True);
      NewConsole.TextView.SetAnchors
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
