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

with Fonts;
with Canvas;
with Basics; use Basics;
with BoundsCalc; use BoundsCalc;
with GUIMouse; use GUIMouse;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body YellowBlue.Window is

   TitleBarHeight   : constant Integer := 24;
   LineWidth        : constant Integer := 1;
   BorderSpaceWidth : constant Integer := 4;
   BorderWidth      : constant Integer := LineWidth*2+BorderSpaceWidth;
   CornerSize       : constant Integer := 2*BorderWidth;
   TopBarHeight     : constant Integer := TitleBarHeight+2*BorderWidth;

   NormalBackgroundColor     : constant Canvas.Color_Type := 16#7F7F7F7F#;
   NormalBorderLineColor     : constant Canvas.Color_Type := 16#FF000000#;
   NormalBorderEdgeLineColor : constant Canvas.Color_Type := 16#FFFFFFFF#;
   NormalClientColor         : constant Canvas.Color_Type := 16#7F00007F#;
   NormalTitleBarColor       : constant Canvas.Color_Type := 16#7F00007F#;

   FocussedBackgroundColor     : constant Canvas.Color_Type := 16#FF7F7F7F#;
   FocussedBorderLineColor     : constant Canvas.Color_Type := 16#FFFFFFFF#;
   FocussedBorderEdgeLineColor : constant Canvas.Color_Type := 16#FF00FF00#;
   FocussedClientColor         : constant Canvas.Color_Type := 16#7F007F00#;
   FocussedTitleBarColor       : constant Canvas.Color_Type := 16#7F007F00#;

   type Window_Type is new GUI.Window.Window_Type with
      record
         TopLeftCorner     : GUI.Canvas_ClassAccess;
         TopBar            : GUI.Canvas_ClassAccess;
         TopRightCorner    : GUI.Canvas_ClassAccess;
         LeftBar           : GUI.Canvas_ClassAccess;
         RightBar          : GUI.Canvas_ClassAccess;
         BottomLeftCorner  : GUI.Canvas_ClassAccess;
         BottomRightCorner : GUI.Canvas_ClassAccess;
         BottomBar         : GUI.Canvas_ClassAccess;
         ClientArea        : GUI.Canvas_ClassAccess;
         TitleCanvas       : GUI.Canvas_ClassAccess;
         Font              : Fonts.Font_ClassAccess;
      end record;
   type Window_Access is access all Window_Type;

   overriding
   function MouseDown
     (Item   : access Window_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer)
      return Boolean;

   overriding
   procedure MouseUp
     (Item   : access Window_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer);

   overriding
   procedure MouseMove
     (Item : access Window_Type;
      X    : Integer;
      Y    : Integer);

   overriding
   procedure Finalize
     (Item : access Window_Type);

   overriding
   procedure Focus
     (Item : access Window_Type);

   overriding
   procedure Defocus
     (Item : access Window_Type);

   overriding
   procedure SetCaption
     (Window  : access Window_Type;
      Caption : Unbounded_String);
   ---------------------------------------------------------------------------

   procedure Finalize
     (Item : access Window_Type) is
   begin

      Fonts.Release(Item.Font);
      GUI.Window.Finalize
        (Item => GUI.Window.Window_Access(Item));

   end Finalize;
   ---------------------------------------------------------------------------

   function MouseDown
     (Item   : access Window_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer)
      return Boolean is

   begin

      If Button=LeftButton then

         if Y<BorderWidth then

            if X<CornerSize then
               Item.StartChange
                 (RefX => X,
                  RefY => Y,
                  Mode => GUI.Window.WindowChangeModeSizeTopLeft);
               return True;
            end if;

            if X>=Item.GetBounds.Width-CornerSize then
               Item.StartChange
                 (RefX => X,
                  RefY => Y,
                  Mode => GUI.Window.WindowChangeModeSizeTopRight);
               return True;
            end if;

            Item.StartChange
              (RefX => X,
               RefY => Y,
               Mode => GUI.Window.WindowChangeModeSizeTop);
            return True;

         end if;

         if Y>=Item.GetBounds.Height-BorderWidth then

            if X<CornerSize then
               Item.StartChange
                 (Refx => X,
                  Refy => Y,
                  Mode => GUI.Window.WindowChangeModeSizeBottomLeft);
               return True;
            end if;

            if X>=Item.GetBounds.Width-CornerSize then
               Item.StartChange
                 (Refx => X,
                  Refy => Y,
                  Mode => GUI.Window.WindowChangeModeSizeBottomRight);
               return True;
            end if;

            Item.StartChange
              (RefX => X,
               RefY => Y,
               Mode => GUI.Window.WindowChangeModeSizeBottom);
            return True;

         end if;

         if X<BorderWidth then

            if Y<CornerSize then
               Item.StartChange
                 (Refx => X,
                  Refy => Y,
                  Mode => GUI.Window.WindowChangeModeSizeTopLeft);
               return True;
            end if;

            if Y>=Item.GetBounds.Height-CornerSize then
               Item.StartChange
                 (Refx => X,
                  Refy => Y,
                  Mode => GUI.Window.WindowChangeModeSizeBottomLeft);
               return True;
            end if;

            Item.StartChange
              (RefX => X,
               RefY => Y,
               Mode => GUI.Window.WindowChangeModeSizeLeft);
            return True;

         end if;

         if X>=Item.GetBounds.Width-BorderWidth then

            if Y<CornerSize then
               Item.StartChange
                 (Refx => X,
                  Refy => Y,
                  Mode => GUI.Window.WindowChangeModeSizeTopRight);
               return True;
            end if;

            if Y>=Item.GetBounds.Height-CornerSize then
               Item.StartChange
                 (Refx => X,
                  Refy => Y,
                  Mode => GUI.Window.WindowChangeModeSizeBottomRight);
               return True;
            end if;

            Item.StartChange
              (RefX => X,
               RefY => Y,
               Mode => GUI.Window.WindowChangeModeSizeRight);
            return True;

         end if;

         if Y<BorderWidth+TitleBarHeight then
            Item.StartChange
              (Refx => X,
               Refy => Y,
               Mode => GUI.Window.WindowChangeModeMove);
            return True;
         end if;

      end if;

      return True;

   end MouseDown;
   ---------------------------------------------------------------------------

   procedure MouseUp
     (Item   : access Window_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer) is
      pragma Unreferenced(Button);
   begin
      Item.ApplyChange
        (Refx => X,
         Refy => Y);
      Item.StopChange;
   end MouseUp;
   ---------------------------------------------------------------------------

   procedure MouseMove
     (Item : access Window_Type;
      X    : Integer;
      Y    : Integer) is
   begin
      Item.ApplyChange
        (Refx => X,
         Refy => Y);
   end MouseMove;
   ---------------------------------------------------------------------------
   procedure DrawTitleCanvas
     (Window : access Window_Type) is

      use type Fonts.Font_ClassAccess;

   begin
      if Window.TitleCanvas/=null then
         FreeCanvas(Window.TitleCanvas);
      end if;

      if Window.Font/=null then

         declare
            Caption   : Unbounded_String := Window.GetCaption;
            TextWidth : constant Integer := Window.Font.TextWidth(Caption);
         begin

            Window.TitleCanvas:=Window.NewCanvas
              (Height => TitleBarHeight,
               Width  => TextWidth);
            Window.TitleCanvas.Clear
              (Color => 16#00000000#);

            Window.Font.TextOut
              (Canvas => Canvas.Canvas_ClassAccess(Window.TitleCanvas),
               X => 0.0,
               Y => 0.0,
               Text => Caption,
               Color => 16#FFFFFFFF#);

            Window.TitleCanvas.SetBounds
              (Top     => BorderWidth+1,
               Left    => BorderWidth+2,
               Height  => TitleBarHeight,
               Width   => TextWidth,
               Visible => True);

            Window.TitleCanvas.SetAnchors
              (Top    => False,
               Left   => False,
               Right  => False,
               Bottom => False);

         end;
      end if;

   end;
   ---------------------------------------------------------------------------

   procedure DrawCanvasse
     (Window : access Window_Type) is

      TitleBarColor       : Canvas.Color_Type;
      BorderLineColor     : Canvas.Color_Type;
      BorderEdgeLineColor : Canvas.Color_Type;
      BackgroundColor     : Canvas.Color_Type;
      ClientColor         : Canvas.Color_Type;

   begin

      if Window.Focussed then
         TitleBarColor       := FocussedTitleBarColor;
         BorderLineColor     := FocussedBorderLineColor;
         BorderEdgeLineColor := FocussedBorderEdgeLineColor;
         BackgroundColor     := FocussedBackgroundColor;
         ClientColor         := FocussedClientColor;
      else
         TitleBarColor       := NormalTitleBarColor;
         BorderLineColor     := NormalBorderLineColor;
         BorderEdgeLineColor := NormalBorderEdgeLineColor;
         BackgroundColor     := NormalBackgroundColor;
         ClientColor         := NormalClientColor;
      end if;

      DrawTitleCanvas(Window);

      if Window.TopLeftCorner/=null then
         FreeCanvas(Window.TopLeftCorner);
      end if;

      Window.TopLeftCorner:=Window.NewCanvas
        (Height => TopBarHeight,
         Width  => CornerSize);

      Window.TopLeftCorner.Clear
        (Color => BackgroundColor);
      Window.TopLeftCorner.HorzLine
        (X     => 0,
         Y     => 0,
         Width => CornerSize,
         Color => BorderLineColor);
      Window.TopLeftCorner.VertLine
        (X      => 0,
         Y      => 1,
         Height => TopBarHeight-1,
         Color  => BorderLineColor);
      Window.TopLeftCorner.HorzLine
        (X      => LineWidth+BorderSpaceWidth,
         Y      => LineWidth+BorderSpaceWidth,
         Width  => CornerSize-LineWidth-BorderSpaceWidth,
         Color  => BorderLineColor);
      Window.TopLeftCorner.VertLine
        (X      => LineWidth+BorderSpaceWidth,
         Y      => LineWidth+BorderSpaceWidth+1,
         Height => TopbarHeight-LineWidth-BorderSpaceWidth-BorderWidth,
         Color  => BorderLineColor);
      Window.TopLeftCorner.HorzLine
        (X      => LineWidth+BorderSpaceWidth+1,
         Y      => TopBarHeight-BorderWidth,
         Width  => CornerSize-LineWidth-BorderSpaceWidth-1,
         Color  => BorderLineColor);
      Window.TopLeftCorner.HorzLine
        (X      => LineWidth+BorderSpaceWidth,
         Y      => TopBarHeight-1,
         Width  => CornerSize-LineWidth-BorderSpaceWidth,
         Color  => BorderLineColor);
      Window.TopLeftCorner.HorzLine
        (X     => 1,
         Y     => CornerSize-1,
         Width => BorderSpaceWidth,
         Color => BorderEdgeLineColor);
      Window.TopLeftCorner.VertLine
        (X      => CornerSize-1,
         Y      => 1,
         Height => BorderSpaceWidth,
         Color  => BorderEdgeLineColor);
      Window.TopLeftCorner.SetBounds
        (Top     => 0,
         Left    => 0,
         Height  => TopBarHeight,
         Width   => CornerSize,
         Visible => True);
      Window.TopLeftCorner.SetAnchors
        (Top    => True,
         Left   => True,
         Right  => False,
         Bottom => False);
      ------------------------------------------------------------------------

      if Window.TopBar/=null then
         FreeCanvas(Window.TopBar);
      end if;

      Window.TopBar:=Window.NewCanvas
        (Height => TopBarHeight,
         Width  => 1);

      Window.TopBar.Clear
        (Color => BackgroundColor);
      Window.TopBar.VertLine
        (Y      => 0,
         X      => BorderWidth,
         Height => TitleBarHeight,
         Color  => TitleBarColor);
      Window.TopBar.Image(0,0)
        :=BorderLineColor;
      Window.TopBar.Image(LineWidth+BorderSpaceWidth,0)
        :=BorderLineColor;
      Window.TopBar.Image(TopBarHeight-BorderSpaceWidth-LineWidth-1,0)
        :=BorderLineColor;
      Window.TopBar.Image(TopBarHeight-1,0)
        :=BorderLineColor;
      Window.TopBar.SetBounds
        (Top     => 0,
         Left    => CornerSize,
         Height  => TopBarHeight,
         Width   => Window.GetBounds.Width-2*CornerSize,
         Visible => True);
      Window.Topbar.SetAnchors
        (Top    => True,
         Left   => True,
         Right  => True,
         Bottom => False);
      ------------------------------------------------------------------------

      if Window.TopRightCorner/=null then
         FreeCanvas(Window.TopRightCorner);
      end if;

      Window.TopRightCorner:=Window.NewCanvas
        (Height => TopBarHeight,
         Width  => CornerSize);

      Window.TopRightCorner.Clear
        (Color => BackgroundColor);
      Window.TopRightCorner.HorzLine
        (X      => 0,
         Y      => 0,
         Width  => CornerSize,
         Color  => BorderLineColor);
      Window.TopRightCorner.VertLine
        (X      => CornerSize-1,
         Y      => 1,
         Height => TopbarHeight-1,
         Color  => BorderLineColor);
      Window.TopRightCorner.HorzLine
        (X      => 0,
         Y      => LineWidth+BorderSpaceWidth,
         Width  => CornerSize-BorderWidth,
         Color  => BorderLineColor);
      Window.TopRightCorner.VertLine
        (X      => CornerSize-BorderWidth,
         Y      => LineWidth+BorderSpaceWidth,
         Height => TopbarHeight-2*BorderWidth+1,
         Color  => BorderLineColor);
      Window.TopRightCorner.HorzLine
        (X      => 0,
         Y      => TopBarHeight-1,
         Width  => CornerSize-BorderWidth+1,
         Color  => BorderLineColor);
      Window.TopRightCorner.HorzLine
        (X      => 0,
         Y      => TopBarHeight-BorderWidth,
         Width  => CornerSize-BorderWidth+1,
         Color  => BorderLineColor);
      Window.TopRightCorner.VertLine
        (X      => 0,
         Y      => LineWidth,
         Height => BorderSpaceWidth,
         Color  => BorderEdgeLineColor);
      Window.TopRightCorner.HorzLine
        (X      => CornerSize-LineWidth-BorderSpaceWidth,
         Y      => CornerSize-1,
         Width  => BorderSpaceWidth,
         Color  => BorderEdgeLineColor);
      Window.TopRightCorner.SetBounds
        (Top     => 0,
         Left    => Window.GetBounds.Width-CornerSize,
         Height  => TopBarHeight,
         Width   => CornerSize,
         Visible => True);
      Window.TopRightCorner.SetAnchors
        (Top    => True,
         Left   => False,
         Right  => True,
         Bottom => False);
      ------------------------------------------------------------------------

      if Window.LeftBar/=null then
         FreeCanvas(Window.LeftBar);
      end if;

      Window.LeftBar:=Window.NewCanvas
        (Height => 1,
         Width  => BorderWidth);
      Window.LeftBar.Clear
        (Color => BackgroundColor);
      Window.LeftBar.Image(0,0):=BorderLineColor;
      Window.LeftBar.Image(0,BorderWidth-1):=BorderLineColor;
      Window.LeftBar.SetBounds
        (Top     => TopBarHeight,
         Left    => 0,
         Height  => Window.GetBounds.Height-TopBarHeight-CornerSize,
         Width   => BorderWidth,
         Visible => True);
      Window.LeftBar.SetAnchors
        (Top    => True,
         Left   => True,
         Right  => False,
         Bottom => True);
      ------------------------------------------------------------------------

      if Window.RightBar/=null then
         FreeCanvas(Window.RightBar);
      end if;

      Window.RightBar:=Window.NewCanvas
        (Height => 1,
         Width  => BorderWidth);
      Window.RightBar.Clear
        (Color => BackgroundColor);
      Window.RightBar.Image(0,0):=BorderLineColor;
      Window.RightBar.Image(0,BorderWidth-1):=BorderLineColor;
      Window.RightBar.SetBounds
        (Top     => TopBarHeight,
         Left    => Window.GetBounds.Width-BorderWidth,
         Height  => Window.GetBounds.Height-TopBarHeight-CornerSize,
         Width   => BorderWidth,
         Visible => True);
      GUI.SetAnchors
        (Canvas => Window.RightBar,
         Top    => True,
         Left   => False,
         Right  => True,
         Bottom => True);
      ------------------------------------------------------------------------

      if Window.BottomLeftCorner/=null then
         FreeCanvas(Window.BottomLeftCorner);
      end if;

      Window.BottomLeftCorner:=Window.NewCanvas
        (Height => CornerSize,
         Width  => CornerSize);
      Window.BottomLeftCorner.Clear
        (Color => BackgroundColor);
      Window.BottomLeftCorner.VertLine
        (X      => 0,
         Y      => 0,
         Height => CornerSize,
         Color  => BorderLineColor);
      Window.BottomLeftCorner.HorzLine
        (X      => 1,
         Y      => CornerSize-1,
         Width  => CornerSize-1,
         Color  => BorderLineColor);
      Window.BottomLeftCorner.VertLine
        (X      => BorderWidth-1,
         Y      => 0,
         Height => CornerSize-BorderWidth+1,
         Color  => BorderLineColor);
      Window.BottomLeftCorner.HorzLine
        (X      => BorderWidth,
         Y      => CornerSize-BorderWidth,
         Width  => CornerSize-BorderWidth,
         Color  => BorderLineColor);
      Window.BottomLeftCorner.HorzLine
        (X      => LineWidth,
         Y      => 0,
         Width  => BorderSpaceWidth,
         Color  => BorderEdgeLineColor);
      Window.BottomLeftCorner.VertLine
        (X      => CornerSize-1,
         Y      => CornerSize-LineWidth-BorderSpaceWidth,
         Height => BorderSpaceWidth,
         Color  => BorderEdgeLineColor);
      Window.BottomLeftCorner.Bar
        (X      => BorderWidth,
         Y      => 0,
         Height => CornerSize-BorderWidth,
         Width  => CornerSize-BorderWidth,
         Color  => 0);
      Window.BottomLeftCorner.SetBounds
        (Top     => Window.GetBounds.Height-CornerSize,
         Left    => 0,
         Height  => CornerSize,
         Width   => CornerSize,
         Visible => True);
      Window.BottomLeftCorner.SetAnchors
        (Top    => False,
         Left   => True,
         Right  => False,
         Bottom => True);
      ------------------------------------------------------------------------

      if Window.BottomBar/=null then
         FreeCanvas(Window.BottomBar);
      end if;

      Window.BottomBar:=Window.NewCanvas
        (Height => BorderWidth,
         Width  => 1);
      Window.BottomBar.Clear
        (Color => BackgroundColor);

      Window.BottomBar.Image(0,0)             := BorderLineColor;
      Window.BottomBar.Image(BorderWidth-1,0) := BorderLineColor;

      Window.BottomBar.SetBounds
        (Top     => Window.GetBounds.Height-BorderWidth,
         Left    => CornerSize,
         Height  => BorderWidth,
         Width   => Window.GetBounds.Width-2*CornerSize,
         Visible => True);
      GUI.SetAnchors
        (Canvas => Window.BottomBar,
         Top    => False,
         Left   => True,
         Right  => True,
         Bottom => True);
      ------------------------------------------------------------------------

      if Window.BottomRightCorner/=null then
         FreeCanvas(Window.BottomRightCorner);
      end if;

      Window.BottomRightCorner:=Window.NewCanvas
        (Height => CornerSize,
         Width  => CornerSize);
      Window.BottomRightCorner.Clear
        (Color => BackgroundColor);
      Window.BottomRightCorner.VertLine
        (X      => CornerSize-1,
         Y      => 0,
         Height => CornerSize,
         Color  => BorderLineColor);
      Window.BottomRightCorner.HorzLine
        (X      => 0,
         Y      => CornerSize-1,
         Width  => CornerSize-1,
         Color  => BorderLineColor);
      Window.BottomRightCorner.VertLine
        (X      => CornerSize-BorderWidth,
         Y      => 0,
         Height => CornerSize-BorderWidth+1,
         Color  => BorderLineColor);
      Window.BottomRightCorner.HorzLine
        (X      => 0,
         Y      => CornerSize-BorderWidth,
         Width  => CornerSize-BorderWidth,
         Color  => BorderLineColor);
      Window.BottomRightCorner.HorzLine
        (X      => CornerSize-LineWidth-BorderSpaceWidth,
         Y      => 0,
         Width  => BorderSpaceWidth,
         Color  => BorderEdgeLineColor);
      Window.BottomRightCorner.VertLine
        (X      => 0,
         Y      => CornerSize-LineWidth-BorderSpaceWidth,
         Height => BorderSpaceWidth,
         Color  => BorderEdgeLineColor);
      Window.BottomRightCorner.Bar
        (X      => 0,
         Y      => 0,
         Height => CornerSize-BorderWidth,
         Width  => CornerSize-BorderWidth,
         Color  => 0);
      Window.BottomRightCorner.SetBounds
        (Top     => Window.GetBounds.Height-CornerSize,
         Left    => Window.GetBounds.Width-CornerSize,
         Height  => CornerSize,
         Width   => CornerSize,
         Visible => True);
      Window.BottomRightCorner.SetAnchors
        (Top    => False,
         Left   => False,
         Right  => True,
         Bottom => True);
      ------------------------------------------------------------------------

      if Window.ClientArea/=null then
         FreeCanvas(Window.ClientArea);
      end if;

      Window.ClientArea:=Window.NewCanvas
        (Height => 1,
         Width  => 1);
      Window.ClientArea.Clear
        (Color => ClientColor);
      Window.ClientArea.SetBounds
        (Top     => TopBarHeight,
         Left    => BorderWidth,
         Height  => Window.GetBounds.Height-TopBarHeight-BorderWidth,
         Width   => Window.GetBounds.Width-2*BorderWidth,
         Visible => True);
      Window.ClientArea.SetAnchors
        (Top    => True,
         Left   => True,
         Right  => True,
         Bottom => True);
      ------------------------------------------------------------------------
   end DrawCanvasse;
   ---------------------------------------------------------------------------

   procedure SetCaption
     (Window  : access Window_Type;
      Caption : Unbounded_String) is
   begin
      GUI.Window.Window_Access(Window).SetCaption(Caption);
      DrawTitleCanvas(Window);
   end SetCaption;
   ---------------------------------------------------------------------------

   procedure Focus
     (Item : access Window_Type) is
   begin
      DrawCanvasse(Item);
      GUI.Window.Window_Access(Item).Focus;
   end Focus;
   ---------------------------------------------------------------------------

   procedure Defocus
     (Item : access Window_Type) is
   begin
      DrawCanvasse(Item);
   end Defocus;
   ---------------------------------------------------------------------------

   function NewWindow
     (Parent : Object_ClassAccess)
      return GUI.Window.Window_ClassAccess is

      NewWindow : Window_Access;

   begin
      NewWindow:=new Window_Type;

      GUI.Window.Initialize
        (Item   => GUI.Window.Window_Access(NewWindow),
         Parent => Parent);

      NewWindow.Font:=Fonts.Lookup
        (Name       => U("Vera"),
         Size       => 18,
         Attributes => Fonts.NoAttributes);
      ------------------------------------------------------------------------
      DrawCanvasse(NewWindow);

      NewWindow.TopHeightConstraint:=
        (MinValueConstraint => ConstraintConstant,
         MinValueConstant   => 0,
         MaxValueConstraint => ConstraintUsingParentSize,
         MaxValueConstant   => 10,
         MinSizeConstraint  => ConstraintConstant,
         MinSizeConstant    => 100,
         MaxSizeConstraint  => ConstraintUsingParentSize,
         MaxSizeConstant    => 0);
      NewWindow.LeftWidthConstraint:=
        (MinValueConstraint => ConstraintUsingSize,
         MinValueConstant   => 10,
         MaxValueConstraint => ConstraintUsingParentSize,
         MaxValueConstant   => 10,
         MinSizeConstraint  => ConstraintConstant,
         MinSizeConstant    => 100,
         MaxSizeConstraint  => ConstraintUsingParentSize,
         MaxSizeConstant    => 0);
      ------------------------------------------------------------------------
      declare
         Client : GUI.Object_Access;
      begin
         Client:=new GUI.Object_Type;
         GUI.Initialize
           (Item   => Client,
            Parent => Object_ClassAccess(NewWindow));
         NewWindow.SetClient(Object_ClassAccess(Client));

         Client.SetBounds
           (Top     => TopBarHeight,
            Left    => BorderWidth,
            Height  => -TopBarHeight-BorderWidth,
            Width   => -2*BorderWidth,
            Visible => True);

         Client.SetAnchors
           (Top    => True,
            Left   => True,
            Right  => True,
           Bottom => True);
      end;

      return GUI.Window.Window_ClassAccess(NewWindow);

   end NewWindow;
   ---------------------------------------------------------------------------

end YellowBlue.Window;
