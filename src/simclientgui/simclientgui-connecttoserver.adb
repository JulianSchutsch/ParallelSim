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
with GUI.Window;
with BoundsCalc; use BoundsCalc;
with Basics; use Basics;
with SimClient.Front;

package body SimClientGUI.ConnectToServer is

   MessageWindowHeight : constant:=240;
   MessageWindowWidth  : constant:=320;
   Enabled       : Boolean:=False;
   MessageWindow : GUI.Window.Window_ClassAccess:=null;

   procedure Connect is
   begin
      Put_Line("Connected");
   end Connect;
   ---------------------------------------------------------------------------

   -- This needs to be moved to a different package maybe?
   procedure Disconnect is
   begin
      Put_Line("Disconnected:::");
   end Disconnect;
   ---------------------------------------------------------------------------

   procedure FailedConnect
     (Retry : out Boolean) is
   begin
      Put_Line("Failed Connect, Retry");
      Retry := True;
   end FailedConnect;
   ---------------------------------------------------------------------------

   procedure Enable
     (Configuration : Config.Config_Type) is

      GUIBounds : constant Bounds_Type:=GUIContext.WindowArea.GetBounds;

   begin

      if Enabled then
         raise ReenabledGUIModule with "ConnectToServer";
      end if;
      Put_Line("SimClientGUI.ConnectToServer.Enable");
      MessageWindow := ThemeImplementation.NewWindow(GUIContext.WindowArea);
      MessageWindow.SetBounds
        (Top     => (GUIBounds.Height-MessageWindowHeight)/2-1,
         Left    => (GUIBounds.Width-MessageWindowWidth)/2-1,
         Height  => MessageWindowHeight,
         Width   => MessageWindowWidth,
         Visible => True);
      MessageWindow.SetCaption
        (Caption => U("Connecting..."));
      SimClient.Front.OnConnect       := Connect'Access;
      SimClient.Front.OnDisconnect    := Disconnect'Access;
      SimClient.Front.OnFailedConnect := FailedConnect'Access;
      SimClient.Front.Connect(Configuration);
      Enabled:=True;

   end Enable;
   ---------------------------------------------------------------------------

   procedure Disable is
   begin
      if not Enabled then
         raise RedisabledGUIModule with "ConnectToServer";
      end if;
      MessageWindow.Free;
      Enabled:=False;
   end Disable;
   ---------------------------------------------------------------------------

end SimClientGUI.ConnectToServer;
