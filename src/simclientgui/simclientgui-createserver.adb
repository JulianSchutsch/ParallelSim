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

with GUI.Console;
with GUI.GroupBox;
with GUI.Window;
with GUI.Label;
with GUI.Button;
with Basics; use Basics;
with BoundsCalc; use BoundsCalc;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with SimConfig;
with SimConfig.Visual;

with SimClient.CreateServer;

with SimClientGUI.ConnectToServer;
with SimClientGUI.MainMenu;

package body SimClientGUI.CreateServer is

   Enabled : Boolean:=False;

   Configuration          : Config.Config_Type;
   ExecOutputGroup        : GUI.GroupBox.GroupBox_ClassAccess    := null;
   ExecOutput             : GUI.Console.Console_ClassAccess      := null;
   SupplementConfigGroup  : GUI.GroupBox.GroupBox_ClassAccess    := null;
   SupplementConfigArray  : SimConfig.ConfigArray_Access         := null;
   SupplementElementsPage : SimConfig.Visual.ElementsPage_Access := null;
   FailureWindow          : GUI.Window.Window_ClassAccess        := null;
   ButtonAbort            : GUI.Button.Button_ClassAccess        := null;
   ButtonRetry            : GUI.Button.Button_ClassAccess        := null;
   SupplementConfig       : Config.Config_Type;

   procedure Resize
     (CallBackObject : AnyObject_ClassAccess) is
      pragma Unreferenced(CallBackObject);
      use type GUI.GroupBox.GroupBox_ClassAccess;

      Bounds : constant Bounds_Type:=GUIContext.WindowArea.GetBounds;

   begin
      ExecOutputGroup.SetBounds
        (Top     => Bounds.Height/2,
         Left    => 10,
         Height  => Bounds.Height-Bounds.Height/2-42,
         Width   => Bounds.Width-20,
         Visible => True);
      if SupplementConfigGroup/=null then
         SupplementConfigGroup.SetBounds
           (Top     => 10,
            Left    => 10,
            Height  => Bounds.Height/2-20,
            Width   => Bounds.Width-20,
            Visible => True);
      end if;
   end Resize;
   ---------------------------------------------------------------------------

   procedure CleanupFailureComponents is
      use type GUI.GroupBox.GroupBox_ClassAccess;
      use type GUI.Button.Button_ClassAccess;
   begin
      Put_Line("CleanupFail");
      if SupplementConfigGroup/=null then
         SupplementConfigGroup.Free;
         SupplementConfigGroup:=null;
         SimConfig.FreeConfigArray(SupplementConfigArray);
      end if;
      if ButtonAbort/=null then
         ButtonAbort.Free;
         ButtonAbort:=null;
      end if;
      if ButtonRetry/=null then
         ButtonRetry.Free;
         ButtonRetry:=null;
      end if;
      Put_Line("CleanupFail//");
   end CleanupFailureComponents;
   ---------------------------------------------------------------------------

   procedure CreateServerMessage
     (Message : Unbounded_String) is
   begin
      ExecOutput.WriteLine(Message,16#FFFFFFFF#);
   end CreateServerMessage;
   ---------------------------------------------------------------------------

   procedure CreateServerSuccessASync
     (Item : GUI.Object_ClassAccess) is
      pragma Unreferenced(Item);
   begin
      Disable;
      SimClientGUI.ConnectToServer.Enable(Configuration);
   end CreateServerSuccessAsync;
   ---------------------------------------------------------------------------

   procedure CreateServerSuccess is
   begin
      ExecOutput.WriteLine(U("Success..."),16#FFFFFFFF#);
      GUIContext.WindowArea.AddASync(CreateServerSuccessASync'Access);
   end CreateServerSuccess;
   ---------------------------------------------------------------------------

   procedure FailureWindowOkASync
     (Item : GUI.Object_ClassAccess) is
      pragma Unreferenced(Item);
   begin
      FailureWindow.Free;
      FailureWindow:=null;
   end FailureWindowOkASync;
   ---------------------------------------------------------------------------

   procedure FailureWindowOkClick
     (CallBackObject : AnyObject_ClassAccess) is
      pragma Unreferenced(CallBackObject);
   begin
      FailureWindow.AddASync(FailureWindowOkASync'Access);
   end FailureWindowOkClick;
   ---------------------------------------------------------------------------

   procedure ButtonRetryClick
     (CallBackObject : AnyObject_ClassAccess) is
      pragma Unreferenced(CallBackObject);
      SupplementConfig : Config.Config_Type;
   begin
      Put_Line("ButtonRetryClick");
      SupplementElementsPage.GetConfig(SupplementConfig);
      Put_Line(".Retry");
      SimClient.CreateServer.Retry(SupplementConfig);
      Put_Line("ButtonRetryClick//");
   end ButtonRetryClick;
   ---------------------------------------------------------------------------

   procedure AbortASync
     (Item : GUI.Object_ClassAccess) is
      pragma Unreferenced(Item);
   begin
      Disable;
      SimClientGUI.MainMenu.Enable;
   end AbortASync;
   ---------------------------------------------------------------------------

   procedure ButtonAbortClick
     (CallBackObject : AnyObject_ClassAccess) is
      pragma Unreferenced(CallBackObject);
   begin
      ButtonAbort.AddASync(AbortASync'Access);
   end ButtonAbortClick;
   ---------------------------------------------------------------------------

   procedure CreateServerFailureASync
     (Item : GUI.Object_ClassAccess) is
      pragma Unreferenced(Item);
   begin
      Put_Line("CreateServerFailure");
      CleanupFailureComponents;
      ExecOutput.WriteLine(U("Failure..."),16#FFFFFFFF#);
      if SupplementConfig.Is_Empty then
         Put_Line("No SupplementConfig");
         -- TODO: Add Failure dialog
         return;
      end if;
      SupplementConfigGroup:=ThemeImplementation.NewGroupBox
        (GUIContext.WindowArea);
      SupplementConfigGroup.SetCaption(U("Supplement Configuration"));
      SupplementConfigArray:=SimConfig.CreateConfigArrayFromConfiguration(SupplementConfig);
      SupplementElementsPage:=SimConfig.Visual.CreateElementsPage
        (Parent  => GUI.Object_ClassAccess(SupplementConfigGroup),
         Theme   => ThemeImplementation,
         Options => SupplementConfigArray);
      SupplementElementsPage.SetBounds
        (Top     => 0,
         Left    => 0,
         Height  => SupplementConfigGroup.GetClientBounds.Height,
         Width   => SupplementConfigGroup.GetClientBounds.Width,
         Visible => True);
      SupplementElementsPage.SetAnchors
        (Top    => True,
         Left   => True,
         Right  => True,
         Bottom => True);

      Put_Line(".FailureWindow");
      declare
         Bounds : constant Bounds_Type:=GUIContext.ModalArea.GetBounds;
         Label  : GUI.Label.Label_ClassAccess;
         Button : GUI.Button.Button_ClassAccess;
      begin
         FailureWindow:=ThemeImplementation.NewWindow(GUIContext.ModalArea);
         FailureWindow.SetBounds
           (Top     => (Bounds.Height-200)/2-1,
            Left    => (Bounds.Width-320)/2-1,
            Height  => 200,
            Width   => 320,
            Visible => True);
         -- TODO: Exchanging SetCaption and SetFocus leads to trouble displaying the caption
         FailureWindow.SetCaption(U("Failure to launch server..."));
         FailureWindow.SetFocus;
         Label:=ThemeImplementation.NewLabel(GUI.Object_ClassAccess(FailureWindow));
         Label.SetBounds
           (Top => 5,
            Left => 5,
            Height => 30,
            Width => 320,
            Visible => True);
         Label.SetCaption(U("Please add missing configuration."));
         Button:=ThemeImplementation.NewButton(GUI.Object_ClassAccess(FailureWindow));
         Button.SetBounds
           (Top => FailureWindow.GetClientBounds.Height-35,
            Left => 5,
            Height => 30,
            Width => FailureWindow.GetClientBounds.Width-10,
            Visible => True);
         Button.SetAnchors
           (Top    => False,
            Left   => True,
            Right  => True,
            Bottom => True);
         Button.SetCaption(U("Ok"));
         Button.OnClick:=FailureWindowOkClick'Access;
      end;

      Put_Line(".Buttons Create");
      declare
         Bounds : constant Bounds_Type:=GUIContext.WindowArea.GetBounds;
      begin
         ButtonAbort:=ThemeImplementation.NewButton(GUIContext.WindowArea);
         ButtonAbort.SetBounds
           (Top     => Bounds.Height-30,
            Left    => 0,
            Height  => 30,
            Width   => 150,
            Visible => True);
         ButtonAbort.SetAnchors
           (Top    => False,
            Left   => True,
            Right  => False,
            Bottom => True);
         ButtonAbort.SetCaption(U("Abort"));
         ButtonAbort.OnClick:=ButtonAbortClick'Access;
         ButtonRetry:=ThemeImplementation.NewButton(GUIContext.WindowArea);
         ButtonRetry.SetBounds
           (Top     => Bounds.Height-30,
            Left    => Bounds.Width-150,
            Height  => 30,
            Width   => 150,
            Visible => True);
         ButtonRetry.SetAnchors
           (Top    => False,
            Left   => False,
            Right  => True,
            Bottom => True);
         ButtonRetry.SetCaption(U("Retry"));
         ButtonRetry.OnClick:=ButtonRetryClick'Access;
      end;
      Resize(null);
      Put_Line("CreateServerFailure//");
   end CreateServerFailureASync;
   ---------------------------------------------------------------------------

   procedure CreateServerFailure
     (SupplementConfig : Config.Config_Type) is
   begin
      Standard.SimClientGUI.CreateServer.SupplementConfig:=SupplementConfig;
      GUIContext.WindowArea.AddASync(CreateServerFailureASync'Access);
   end CreateServerFailure;
   ---------------------------------------------------------------------------

   procedure Enable
     (Configuration : Config.Config_Type) is
   begin
      Put_Line("Enable");
      if Enabled then
         raise ReenabledGUIModule with "CreateProcess";
      end if;
      Standard.SimClientGUI.CreateServer.Configuration:=Configuration;
      ExecOutputGroup:=ThemeImplementation.NewGroupBox(GUIContext.WindowArea);
      ExecOutput:=ThemeImplementation.NewConsole(GUI.Object_ClassAccess(ExecOutputGroup));
      ExecOutput.SetBounds
        (Top     => 5,
         Left    => 5,
         Height  => ExecOutputGroup.GetClientBounds.Height-10,
         Width   => ExecOutputGroup.GetClientBounds.Width-10,
         Visible => True);
      ExecOutput.SetAnchors
        (Top    => True,
         Left   => True,
         Right  => True,
         Bottom => True);
      ExecOutputGroup.SetCaption(U("Distributed System startup output..."));

      GUIContext.WindowArea.OnResize:=Resize'Access;
      Resize(null);

      SimClient.CreateServer.OnMessage:=CreateServerMessage'Access;
      SimClient.CreateServer.OnSuccess:=CreateServerSuccess'Access;
      SimClient.CreateServer.OnFailure:=CreateServerFailure'Access;
      SimClient.CreateServer.Initialize(Configuration);

      Enabled:=True;
      Put_Line("Enable//");
   end Enable;
   ---------------------------------------------------------------------------

   procedure Disable is
      use type GUI.GroupBox.GroupBox_ClassAccess;
   begin
      Put_Line("Disable");
      if not Enabled then
         raise RedisabledGUIModule with "CreateProcess";
      end if;
      CleanUpFailureComponents;

      if ExecOutputGroup/=null then
         ExecOutputGroup.Free;
         ExecOutputGroup:=null;
      end if;

      SimClient.CreateServer.Finalize;
      SimClient.CreateServer.OnMessage:=null;
      SimClient.CreateServer.OnSuccess:=null;
      SimClient.CreateServer.OnFailure:=null;

      GUIContext.WindowArea.OnResize:=null;
      Enabled:=False;
      Put_Line("Disable//");
   end Disable;
   ---------------------------------------------------------------------------

end SimClientGUI.CreateServer;
