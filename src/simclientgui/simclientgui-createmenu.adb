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
with GUI.Label;
with GUI.Edit;
with Basics; use Basics;
with BoundsCalc; use BoundsCalc;

with SimClientGUI.MainMenu;
with SimConfig;
with SimConfig.Visual;

with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body SimClientGUI.CreateMenu is

   type ModuleDesc_Type is
      record
         Description : Unbounded_String;
         FileName    : Unbounded_String;
      end record;
   type ModuleArray_Type is array(Integer range <>) of ModuleDesc_Type;

   Modules : constant ModuleArray_Type:=
     ((Description => U("Distribution"),
       FileName    => U("distribution.cfi")),
      (Description => U("Node"),
       FileName    => U("node.cfi")),
      (Description => U("Front"),
       FileName    => U("front.cfi")));

   type ModuleTab_Type is
      record
         ConfigArray  : SimConfig.ConfigArray_Access         := null;
         Tab          : GUI.TabControl.Tab_ClassAccess       := null;
         ElementsPage : SimConfig.Visual.ElementsPage_Access := null;
      end record;
   type ModuleTabArray_Type is array(Integer range <>) of aliased ModuleTab_Type;
   type ModuleTabArray_Access is access all ModuleTabArray_Type;

   Tabs               : GUI.TabControl.TabControl_ClassAccess;
   Enabled            : Boolean:=False;
   GeneralTab         : GUI.TabControl.Tab_ClassAccess;
   ButtonReturn       : GUI.Button.Button_ClassAccess;
   ButtonLaunch       : GUI.Button.Button_ClassAccess;
   GameTypeGroup      : GUI.GroupBox.GroupBox_ClassAccess;
   NodesGroup         : GUI.GroupBox.GroupBox_ClassAccess;
   ModuleTabs         : ModuleTabArray_Access:=null;

   RadioSinglePlayer              : GUI.RadioButton.RadioButton_ClassAccess;
   RadioMultiPlayerSingleComputer : GUI.RadioButton.RadioButton_ClassAccess;
   RadioMultiPlayerMultiComputer  : GUI.RadioButton.RadioButton_ClassAccess;

   NodeCountLabel  : GUI.Label.Label_ClassAccess;
   NodeCountEdit   : GUI.Edit.Edit_ClassAccess;
   FrontCountLabel : GUI.Label.Label_ClassAccess;
   FrontCountEdit  : GUI.Edit.Edit_ClassAccess;

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

   procedure ClearGameTabs is
      procedure Free is new Ada.Unchecked_Deallocation
        (Object => ModuleTabArray_Type,
         Name   => ModuleTabArray_Access);
   begin
      if ModuleTabs=null then
         return;
      end if;
      for i in ModuleTabs'Range loop
         ModuleTabs(i).ElementsPage.Free;
         ModuleTabs(i).Tab.Free;
      end loop;
      Free(ModuleTabs);
   end ClearGameTabs;
   ------------------------------------------------------------------------

   procedure SelectGameType is

      use type SimConfig.ConfigArray_Access;

   begin
      Put_Line("Select Game Type");
      ClearGameTabs;
      ModuleTabs:=new ModuleTabArray_Type(Modules'Range);
      for i in ModuleTabs'Range loop
         ModuleTabs(i).ConfigArray:=SimConfig.LoadConfig
           (Modules(i).FileName);
         SimConfig.DebugConfigArray(ModuleTabs(i).ConfigArray);
         ModuleTabs(i).Tab:=Tabs.NewTab(Modules(i).Description);
         ModuleTabs(i).ElementsPage:=SimConfig.Visual.CreateElementsPage
           (Parent    => GUI.Object_ClassAccess(ModuleTabs(i).Tab),
            Options   => ModuleTabs(i).ConfigArray,
            Theme     => ThemeImplementation);
         ModuleTabs(i).ElementsPage.SetBounds
           (Top => 0,
            Left => 0,
            Height => ModuleTabs(i).Tab.GetBounds.Height,
            Width => ModuleTabs(i).Tab.GetBounds.Width,
            Visible => True);
         ModuleTabs(i).ElementsPage.SetAnchors
           (Top => True,
            Left => True,
            Right => True,
            Bottom => True);
      end loop;

   end SelectGameType;
   ---------------------------------------------------------------------------

   procedure Enable is
      WindowBounds : constant Bounds_Type:=GUIContext.WindowArea.GetBounds;

      procedure CreateGeneralTab is
      begin

         GeneralTab:=Tabs.NewTab(U("General"));

         GameTypeGroup:=ThemeImplementation.NewGroupBox(GUI.Object_ClassAccess(GeneralTab));
         GameTypeGroup.SetCaption(U("Game type"));

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
         ---------------------------------------------------------------------

         NodesGroup:=ThemeImplementation.NewGroupBox(GUI.Object_ClassAccess(GeneralTab));
         NodesGroup.SetCaption(U("Nodes"));

         NodeCountLabel:=ThemeImplementation.NewLabel(GUI.Object_ClassAccess(NodesGroup));
         NodeCountLabel.SetBounds
           (Top     => 5,
            Left    => 5,
            Height  => 30,
            Width   => 300,
            Visible => True);
         NodeCountLabel.SetCaption(U("Simulation Node Count:"));

         NodeCountEdit:=ThemeImplementation.NewEdit(GUI.Object_ClassAccess(NodesGroup));
         NodeCountEdit.SetBounds
           (Top     => 35,
            Left    => 5,
            Height  => 30,
            Width   => 100,
            Visible => True);
         NodeCountEdit.SetText(U("1"));

         FrontCountLabel:=ThemeImplementation.NewLabel(GUI.Object_ClassAccess(NodesGroup));
         FrontCountLabel.SetBounds
           (Top     => 85,
            Left    => 5,
            Height  => 30,
            Width   => 300,
            Visible => True);
         FrontCountLabel.SetCaption(U("Front Node Count:"));

         FrontCountEdit:=ThemeImplementation.NewEdit(GUI.Object_ClassAccess(NodesGroup));
         FrontCountEdit.SetBounds
           (Top     => 110,
            Left    => 5,
            Height  => 30,
            Width   => 100,
            Visible => True);
         FrontCountEdit.SetText(U("1"));

         GeneralTab.OnResize:=GeneralTabResize'Access;
         GeneralTabResize(null);

      end CreateGeneralTab;

   begin
      Put_Line("Enabled **********************");
      if Enabled then
         raise ReenabledGUIModule with "CreateMenu";
      end if;
      Tabs:=ThemeImplementation.NewTabControl(GUIContext.WindowArea);
      Put_Line("General Tab Create*******");
      CreateGeneralTab;
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

      -- TODO: Could be unnecessary if the above .SetChecked does that
      Put_Line("Select Game Type*********************************");
      SelectGameType;

      Put_Line("WindowAreaResize******************************");
      WindowAreaResize(null);
      Enabled:=True;

   end Enable;
   ---------------------------------------------------------------------------

   procedure Disable is
   begin
      if not Enabled then
         raise RedisabledGUIModule with "CreateMenu";
      end if;
      Put_Line("Disable..***********************:");
      ClearGameTabs;
      Put_Line("Rest******************************");
      Tabs.Free;
      ButtonReturn.Free;
      ButtonLaunch.Free;
      GUIContext.WindowArea.OnResize:=null;
      Enabled:=False;
      Put_Line("Done DISABLE*************");
   end Disable;
   ---------------------------------------------------------------------------

end SimClientGUI.CreateMenu;