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

with GUI.TabControl;
with GUI.Button;
with GUI.GroupBox;
with GUI.RadioButton;
with Basics; use Basics;
with BoundsCalc; use BoundsCalc;

with SimClientGUI.MainMenu;

package body SimClientGUI.CreateMenu is

   Tabs            : GUI.TabControl.TabControl_ClassAccess;
   Enabled         : Boolean:=False;
   GeneralTab      : GUI.TabControl.Tab_ClassAccess;
   DistributionTab : GUI.TabControl.Tab_ClassAccess;pragma Unreferenced(DistributionTab);
   NodeTab         : GUI.TabControl.Tab_ClassAccess;pragma Unreferenced(NodeTab);
   FrontTab        : GUI.TabControl.Tab_ClassAccess;pragma Unreferenced(FrontTab);
   ButtonReturn    : GUI.Button.Button_ClassAccess;
   ButtonLaunch    : GUI.Button.Button_ClassAccess;
   GameTypeGroup   : GUI.GroupBox.GroupBox_ClassAccess;
   NodesGroup      : GUI.GroupBox.GroupBox_ClassAccess;

   RadioSinglePlayer              : GUI.RadioButton.RadioButton_ClassAccess;
   RadioMultiPlayerSingleComputer : GUI.RadioButton.RadioButton_ClassAccess;
   RadioMultiPlayerMultiComputer  : GUI.RadioButton.RadioButton_ClassAccess;

   procedure WindowAreaResize
     (CallBackObject : AnyObject_ClassAccess) is
      pragma Unreferenced(CallBackObject);
      WindowBounds : constant Bounds_Type:=GUIContext.WindowArea.GetBounds;
   begin
      Tabs.SetBounds
        (Top     => 0,
         Left    => 0,
         Height  => WindowBounds.Height-32,
         Width   => WindowBounds.Width,
         Visible => True);
   end WindowAreaResize;
   ---------------------------------------------------------------------------

   procedure ASyncReturn
     (Item : GUI.Object_ClassAccess) is
      pragma Unreferenced(Item);
   begin
      Disable;
      SimClientGUI.MainMenu.Enable;
   end ASyncReturn;
   ---------------------------------------------------------------------------

   procedure ButtonReturnClick
     (CallBackObject : AnyObject_ClassAccess) is
      pragma Unreferenced(CallBackObject);
   begin
      ButtonReturn.AddASync(ASyncReturn'Access);
   end ButtonReturnClick;
   ---------------------------------------------------------------------------

   procedure GeneralTabResize
     (CallBackObject : AnyObject_ClassAccess) is

      pragma Unreferenced(CallBackObject);

      Bounds : constant Bounds_Type:=GeneralTab.GetBounds;

   begin
      GameTypeGroup.SetBounds
        (Top     => 10,
         Left    => 10,
         Height  => Bounds.Height-20,
         Width   => Bounds.Width/2-15,
         Visible => True);
      NodesGroup.SetBounds
        (Top     => 10,
         Left    => Bounds.Width/2+5,
         Height  => Bounds.Height-20,
         Width   => Bounds.Width-Bounds.Width/2-15,
         Visible => True);
   end GeneralTabResize;
   ---------------------------------------------------------------------------

   procedure Enable is
      WindowBounds : constant Bounds_Type:=GUIContext.WindowArea.GetBounds;

      procedure CreateGeneralTab is
      begin

         GeneralTab:=Tabs.NewTab(U("General"));

         GameTypeGroup:=ThemeImplementation.NewGroupBox(GUI.Object_ClassAccess(GeneralTab));
         GameTypeGroup.SetCaption(U("Game type"));

         NodesGroup:=ThemeImplementation.NewGroupBox(GUI.Object_ClassAccess(GeneralTab));
         NodesGroup.SetCaption(U("Nodes"));

         RadioSinglePlayer:=ThemeImplementation.NewRadioButton(GUI.Object_ClassAccess(GameTypeGroup));
         RadioSinglePlayer.SetBounds
           (Top     => 5,
            Left    => 5,
            Height  => 30,
            Width   => 300,
            Visible => True);
         RadioSinglePlayer.SetCaption(U("Single Player"));

         RadioMultiPlayerSingleComputer:=ThemeImplementation.NewRadioButton(GUI.Object_ClassAccess(GameTypeGroup));
         RadioMultiPlayerSingleComputer.SetBounds
           (Top     => 45,
            Left    => 5,
            Height  => 30,
            Width   => 300,
            Visible => True);
         RadioMultiPlayerSingleComputer.SetCaption(U("Multi Player, Single Computer"));
         RadioMultiPlayerSingleComputer.Link(RadioSinglePlayer);

         RadioMultiPlayerMultiComputer:=ThemeImplementation.NewRadioButton(GUI.Object_ClassAccess(GameTypeGroup));
         RadioMultiPlayerMultiComputer.SetBounds
           (Top     => 85,
            Left    => 5,
            Height  => 30,
            Width   => 300,
            Visible => True);
         RadioMultiPlayerMultiComputer.SetCaption(U("Multi Player, Multi Computer"));
         RadioMultiPlayerMultiComputer.Link(RadioSinglePlayer);

         RadioSinglePlayer.SetChecked;

         GeneralTab.OnResize:=GeneralTabResize'Access;
         GeneralTabResize(null);

      end CreateGeneralTab;

   begin
      if Enabled then
         raise ReenabledGUIModule with "CreateMenu";
      end if;
      Tabs:=ThemeImplementation.NewTabControl(GUIContext.WindowArea);
      CreateGeneralTab;
      DistributionTab:=Tabs.NewTab(U("Distribution"));
      NodeTab:=Tabs.NewTab(U("Node"));
      FrontTab:=Tabs.NewTab(U("Front"));
      GUIContext.WindowArea.OnResize:=WindowAreaResize'Access;

      ButtonReturn:=ThemeImplementation.NewButton(GUIContext.WindowArea);
      ButtonReturn.SetBounds
        (Top     => WindowBounds.Height-30,
         Left    => 0,
         Height  => 30,
         Width   => 150,
         Visible => True);
      ButtonReturn.SetAnchors
        (Top    => False,
         Left   => True,
         Right  => False,
         Bottom => True);
      ButtonReturn.SetCaption(U("Return to Menu"));
      ButtonReturn.OnClick:=ButtonReturnClick'Access;

      ButtonLaunch:=ThemeImplementation.NewButton(GUIContext.WindowArea);
      ButtonLaunch.SetBounds
        (Top     => WindowBounds.Height-30,
         Left    => WindowBounds.Width-150,
         Height  => 30,
         Width   => 150,
         Visible => True);
      ButtonLaunch.SetAnchors
        (Top    => False,
         Left   => False,
         Right  => True,
         Bottom => True);
      ButtonLaunch.SetCaption(U("Launch Game"));

      WindowAreaResize(null);
      Enabled:=True;
   end Enable;
   ---------------------------------------------------------------------------

   procedure Disable is
   begin
      if not Enabled then
         raise RedisabledGUIModule with "CreateMenu";
      end if;
      Tabs.Free;
      ButtonReturn.Free;
      ButtonLaunch.Free;
      GUIContext.WindowArea.OnResize:=null;
      Enabled:=False;
   end Disable;
   ---------------------------------------------------------------------------

end SimClientGUI.CreateMenu;
