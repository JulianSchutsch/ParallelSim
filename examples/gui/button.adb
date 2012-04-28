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

-- Revision History
--   28.Mar 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

with GUI;
with GUI.Button;
with GUI.Themes;
with GUI.UseImplementations;
with YellowBlue;
with Config;
with Fonts.Freetype;
with Basics; use Basics;
with BitmapFonts;
with ProcessLoop;

procedure Button is

   GUIImplementation : GUI.Implementation_Type;
   Context           : GUI.Context_ClassAccess;
   Button            : GUI.Button.Button_ClassAccess;
   Theme             : GUI.Themes.Implementation_Type;
   Configuration     : Config.Config_Type;
   Terminated        : Boolean:=False;
   pragma Warnings(Off,Terminated);

   procedure Click
     (CallBackObject : AnyObject_ClassAccess) is
   begin
      Terminated:=True;
   end Click;
   ---------------------------------------------------------------------------

   procedure ContextClose
     (CallBackObject : AnyObject_ClassAccess) is
   begin
      Terminated:=True;
   end ContextClose;
   ---------------------------------------------------------------------------

begin

   GUI.UseImplementations.Register;
   YellowBlue.Register;
   Fonts.Freetype.Register;
   BitmapFonts.Register;

   GUIImplementation := GUI.Implementations.FindAny;
   Theme             := GUI.Themes.Implementations.FindAny;

   Context:=GUIImplementation.NewContext
     (Configuration => Configuration,
      Node          => U(""));
   Context.OnClose:=ContextClose'Unrestricted_Access;

   Button:=Theme.NewButton(Context.WindowArea);
   Button.SetBounds
     (Top     => 10,
      Left    => 10,
      Height  => 30,
      Width   => 100,
      Visible => True);
   Button.SetCaption(U("Close"));
   Button.OnClick:=Click'Unrestricted_Access;

   while not Terminated loop
      ProcessLoop.Process;
   end loop;

   BitmapFonts.Unregister;
   Fonts.Freetype.Unregister;
   YellowBlue.UnRegister;
   GUI.UseImplementations.Unregister;

end Button;
