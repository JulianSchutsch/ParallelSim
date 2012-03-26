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

package body GUI.Themes.YellowBlue.Window is

   TitleBarHeight   : constant Integer := 24;
   LineWidth        : constant Integer := 1;
   BorderSpaceWidth : constant Integer := 4;
   BorderWidth      : constant Integer := LineWidth*2+BorderSpaceWidth;
   CornerSize       : constant Integer := 2*BorderWidth;
   TopBarHeight     : constant Integer := TitleBarHeight*2*BorderWidth;

   type Window_Type is new GUI.Window.Window_Type with
      record
         TopLeftCorner : GUI.Canvas_ClassAccess;
      end record;
   type Window_Access is access all Window_Type;

   function NewWindow
     (Parent : Object_ClassAccess)
      return GUI.Window.Window_ClassAccess is

      NewWindow : Window_Access;

   begin
      NewWindow:=new Window_Type;

      GUI.Window.Initialize
        (Item   => GUI.Window.Window_Access(NewWindow),
         Parent => Parent);

--      SetBounds
--        (Object => Object_ClassAccess(NewWindow),
--         Bounds =>
--           (Top     => 0,
--            Left    => 0,
--            Height  => 100,
--            Width   => 100,
--            Visible => True));

      NewWindow.Context.NewCanvas
        (Object => Object_ClassAccess(NewWindow),
         Height => 10,
         Width  => 10,
         Canvas => NewWindow.TopLeftCorner);
      NewWindow.TopLeftCorner.Clear
        (Color => 16#FFFF00FF#);

      return GUI.Window.Window_ClassAccess(NewWindow);

   end NewWindow;
   --------------------------------------------------------------------------

end GUI.Themes.YellowBlue.Window;
