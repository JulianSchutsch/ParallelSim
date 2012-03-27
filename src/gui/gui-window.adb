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

with Ada.Text_IO; use Ada.Text_IO;

package body GUI.Window is

   procedure StartChange
     (Window : access Window_Type;
      RefX   : Integer;
      RefY   : Integer;
      Mode   : WindowChangeMode_Enum) is
   begin
      Put("StartChange");
      New_Line;
      Window.RefX      := RefX;
      Window.RefY      := RefY;
      Window.RefHeight := Window.Priv.Bounds.Height;
      Window.RefWidth  := Window.Priv.Bounds.Width;
      Window.Mode:=Mode;
   end StartChange;
   ---------------------------------------------------------------------------

   procedure ApplyChange
     (Window : access Window_Type;
      RefX   : Integer;
      RefY   : Integer) is

      Bounds : Bounds_Type renames Window.Priv.Bounds;

   begin
      Put("ApplyChange");
      New_Line;

      case Window.Mode is
         when WindowChangeModeNothing =>
            null;
         when WindowChangeModeMove =>
            GUI.SetBounds
              (Object => Object_ClassAccess(Window),
               Bounds =>
                 (Top     => Bounds.Top+RefY-Window.RefY,
                  Left    => Bounds.Left+RefX-Window.RefX,
                  Height  => Bounds.Height,
                  Width   => Bounds.Width,
                  Visible => Bounds.Visible));
         when WindowChangeModeSizeTopLeft =>
            GUI.SetBounds
              (Object => Object_ClassAccess(Window),
               Bounds =>
                 (Top     => Bounds.Top+RefY-Window.RefY,
                  Left    => Bounds.Left+RefX-Window.RefX,
                  Height  => Bounds.Height-RefY+Window.RefY,
                  Width   => Bounds.Width-RefX+Window.RefX,
                  Visible => Bounds.Visible));
         when WindowChangeModeSizeTop =>
            GUI.SetBounds
              (Object => Object_ClassAccess(Window),
               Bounds =>
                 (Top     => Bounds.Top+RefY-Window.RefY,
                  Left    => Bounds.Left,
                  Height  => Bounds.Height-RefY+Window.RefY,
                  Width   => Bounds.Width,
                  Visible => Bounds.Visible));
         when WindowChangeModeSizeTopRight =>
            GUI.SetBounds
              (Object => Object_ClassAccess(Window),
               Bounds =>
                 (Top     => Bounds.Top+RefY-Window.RefY,
                  Left    => Bounds.Left,
                  Height  => Bounds.Height-RefY+Window.RefY,
                  Width   => Bounds.Width,
                  Visible => Bounds.Visible));
         when WindowChangeModeSizeLeft =>
            GUI.SetBounds
              (Object => Object_ClassAccess(Window),
               Bounds =>
                 (Top     => Bounds.Top,
                  Left    => Bounds.Left+RefX-Window.RefX,
                  Height  => Bounds.Height,
                  Width   => Bounds.Width-RefX+Window.RefX,
                  Visible => Bounds.Visible));
         when WindowChangeModeSizeRight =>
            GUI.SetBounds
              (Object => Object_ClassAccess(Window),
               Bounds =>
                 (Top     => Bounds.Top,
                  Left    => Bounds.Left,
                  Height  => Bounds.Height,
                  Width   => Window.RefWidth+RefX-Window.RefX,
                  Visible => Bounds.Visible));
         when WindowChangeModeSizeBottomLeft =>
            GUI.SetBounds
              (Object => Object_ClassAccess(Window),
               Bounds =>
                 (Top     => Bounds.Top,
                  Left    => Bounds.Left+RefX+Window.RefX,
                  Height  => Window.RefHeight+RefY-Window.RefY,
                  Width   => Bounds.Width-RefX+Window.RefX,
                  Visible => Bounds.Visible));
         when WindowChangeModeSizeBottom =>
            GUI.SetBounds
              (Object => Object_ClassAccess(Window),
               Bounds =>
                 (Top     => Bounds.Top,
                  Left    => Bounds.Left,
                  Height  => Window.RefHeight+RefY-Window.RefY,
                  Width   => Bounds.Width,
                  Visible => Bounds.Visible));
         when WindowChangeModeSizeBottomRight =>
            GUI.SetBounds
              (Object => Object_ClassAccess(Window),
               Bounds =>
                 (Top     => Bounds.Top,
                  Left    => Bounds.Left,
                  Height  => Window.RefHeight+RefY-Window.RefY,
                  Width   => Window.RefWidth+RefX-Window.RefX,
                  Visible => Bounds.Visible));
      end case;

   end ApplyChange;
   ---------------------------------------------------------------------------

   procedure StopChange
     (Window : access Window_Type) is
   begin
      Window.Mode:=WindowChangeModeNothing;
   end StopChange;
   ---------------------------------------------------------------------------

   procedure Initialize
     (Item    : Window_Access;
      Parent  : Object_ClassAccess) is
   begin

      GUI.Initialize
        (Item   => Object_Access(Item),
         Parent => Parent);

   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize
     (Item : access Window_Type) is
   begin

      GUI.Finalize
        (Item => Object_Access(Item));

   end Finalize;
   ---------------------------------------------------------------------------

end GUI.Window;
