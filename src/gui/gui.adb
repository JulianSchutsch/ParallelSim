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

with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body GUI is

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Object_Type,
      Name   => Object_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Canvas_Type'Class,
      Name   => Canvas_ClassAccess);
   ---------------------------------------------------------------------------

   procedure MouseDown
     (Context     : Context_ClassAccess;
      MouseButton : MouseButton_Enum;
      AbsX        : Integer;
      AbsY        : Integer) is
      pragma Unreferenced(Context);
   begin
      case MouseButton is
         when LeftButton =>
            Put("Left");
         when RightButton =>
            Put("Right");
      end case;
      Put(AbsX);
      Put(AbsY);
      New_Line;
   end MouseDown;
   ---------------------------------------------------------------------------

   procedure MouseUp
     (Context     : Context_ClassAccess;
      MouseButton : MouseButton_Enum;
      AbsX        : Integer;
      AbsY        : Integer) is
   begin
      null;
   end MouseUp;
   ---------------------------------------------------------------------------

   procedure MouseMove
     (Context : Context_ClassAccess;
      AbsX    : Integer;
      AbsY    : Integer) is
   begin
      null;
   end MouseMove;
   ---------------------------------------------------------------------------

   procedure Resize
     (Object : Object_ClassAccess) is

      ObjectCursor : Object_ClassAccess;
      CanvasCursor : Canvas_ClassAccess;

   begin

      if Object.Priv.Parent/=null then

         RestoreAnchors
           (Anchors      => Object.Priv.Anchors,
            ClientBounds => Object.Priv.Bounds,
            ParentBounds => Object.Priv.Parent.Priv.Bounds);

         ApplyConstraint
           (Constraint => Object.Priv.TopHeightConstraint,
            Value      => Object.Priv.Bounds.Top,
            Size       => Object.Priv.Bounds.Height,
            OldValue   => Object.Priv.PrevBounds.Top,
            OldSize    => Object.Priv.PrevBounds.Height,
            ParentSize => Object.Priv.Parent.Priv.Bounds.Height);
         ApplyConstraint
           (Constraint => Object.Priv.LeftWidthConstraint,
            Value      => Object.Priv.Bounds.Left,
            Size       => Object.Priv.Bounds.Width,
            OldValue   => Object.Priv.PrevBounds.Left,
            OldSize    => Object.Priv.PrevBounds.Width,
            ParentSize => Object.Priv.Parent.Priv.Bounds.Width);

         NestBounds
           (ParentAbsBounds => Object.Priv.Parent.Priv.AbsBounds,
            RectBounds      => Object.Priv.Bounds,
            ResultBounds    => Object.Priv.AbsBounds);
      else
         Object.Priv.AbsBounds:=
           (AbsTop     => Object.Priv.Bounds.Top,
            AbsLeft    => Object.Priv.Bounds.Left,
            AbsHeight  => Object.Priv.Bounds.Height,
            AbsWidth   => Object.Priv.Bounds.Width,
            AbsSubTop  => 0,
            AbsSubLeft => 0,
            AbsVisible => Object.Priv.Bounds.Visible);
      end if;
      ------------------------------------------------------------------------

      CanvasCursor:=Object.Priv.Canvasse;

      while CanvasCursor/=null loop
         RestoreAnchors
           (Anchors      => CanvasCursor.Anchors,
            ClientBounds => CanvasCursor.Bounds,
            ParentBounds => Object.Priv.Bounds);
         CanvasCursor:=CanvasCursor.Next;
      end loop;
      ------------------------------------------------------------------------

      ObjectCursor:=Object.Priv.FirstChild;

      while ObjectCursor/=null loop
         Resize(ObjectCursor);
         ObjectCursor:=ObjectCursor.Priv.Next;
      end loop;
      ------------------------------------------------------------------------

      Object.Priv.PrevBounds := Object.Priv.Bounds;

   end Resize;
   ---------------------------------------------------------------------------

   procedure Resize
     (Context : Context_ClassAccess;
      Height  : Integer;
      Width   : Integer) is
   begin
      Context.Bounds:=
        (Top     => 0,
         Left    => 0,
         Height  => Height,
         Width   => Width,
         Visible => True);
      Put("Resize Processing");
      New_Line;
      Put(Context.Bounds);
      if Context.WindowArea/=null then
         Put("WindowArea");
         New_Line;
         SetBounds
           (Object => Context.WindowArea,
            Bounds => Context.Bounds);
      end if;
      if Context.ModalArea/=null then
         SetBounds
           (Object => Context.ModalArea,
            Bounds => Context.Bounds);
      end if;
      if Context.ContextMenuArea/=null then
         SetBounds
           (Object => Context.ContextMenuArea,
            Bounds => Context.Bounds);
      end if;
   end Resize;
   ---------------------------------------------------------------------------

   procedure SetAnchors
     (Object : Object_ClassAccess;
      Top    : Boolean;
      Left   : Boolean;
      Right  : Boolean;
      Bottom : Boolean) is
   begin
      Object.Priv.Anchors.Top    := Top;
      Object.Priv.Anchors.Left   := Left;
      Object.Priv.Anchors.Right  := Right;
      Object.Priv.Anchors.Bottom := Bottom;

      if Object.Priv.Parent/=null then
         StoreAnchors
           (Anchors      => Object.Priv.Anchors,
            ClientBounds => Object.Priv.Bounds,
            ParentBounds => Object.Priv.Parent.Priv.Bounds);
      end if;

   end SetAnchors;
   ---------------------------------------------------------------------------

   procedure SetBounds
     (Canvas : Canvas_ClassAccess;
      Bounds : Bounds_Type) is
   begin

      Canvas.Bounds:=Bounds;
      StoreAnchors
        (Anchors => Canvas.Anchors,
         ClientBounds => Canvas.Bounds,
         ParentBounds => Canvas.Object.Priv.Bounds);

   end SetBounds;
   ---------------------------------------------------------------------------

   procedure SetAnchors
     (Canvas : Canvas_ClassAccess;
      Top    : Boolean;
      Left   : Boolean;
      Right  : Boolean;
      Bottom : Boolean) is

   begin

      Canvas.Anchors.Top    := Top;
      Canvas.Anchors.Left   := Left;
      Canvas.Anchors.Right  := Right;
      Canvas.Anchors.Bottom := Bottom;

      StoreAnchors
        (Anchors      => Canvas.Anchors,
         ClientBounds => Canvas.Bounds,
         ParentBounds => Canvas.Object.Priv.Bounds);

   end SetAnchors;
   ---------------------------------------------------------------------------

   procedure SetBounds
     (Object : Object_ClassAccess;
      Bounds : Bounds_Type) is
   begin
      Object.Priv.Bounds:=Bounds;
      Resize(Object);
   end SetBounds;
   ---------------------------------------------------------------------------

   function GetBounds
     (Object : Object_Type'Class)
      return Bounds_Type is
   begin
      return Object.Priv.Bounds;
   end GetBounds;
   ---------------------------------------------------------------------------

   procedure AddCanvas
     (Object : Object_ClassAccess;
      Canvas : Canvas_ClassAccess) is
   begin

      Canvas.Next:=Object.Priv.Canvasse;
      if Canvas.Next/=null then
         Canvas.Next.Last:=Canvas;
      end if;
      Object.Priv.Canvasse:=Canvas;

      Canvas.Object:=Object;

   end AddCanvas;
   ---------------------------------------------------------------------------

   procedure RemoveCanvas
     (Canvas : Canvas_ClassAccess) is

      CanvasVal : Canvas_ClassAccess:=Canvas;

   begin
      if Canvas.Next/=null then
         Canvas.Next.Last:=Canvas.Last;
      end if;
      if Canvas.Last/=null then
         Canvas.Last.Next:=Canvas.Next;
      else
         Canvas.Object.Priv.Canvasse:=Canvas.Next;
      end if;

      Free(CanvasVal);

   end RemoveCanvas;
   ---------------------------------------------------------------------------

   procedure Initialize
     (Item   : Object_Access;
      Parent : Object_ClassAccess) is

     Par : Object_ClassAccess;

   begin
      Par := Parent;
      while Par.Client/=null loop
         Par:=Par.Client;
      end loop;

      Item.Priv.Parent:=Par;

      if Par/=null then

         Item.Context := Par.Context;

         Item.Priv.Next:=Par.Priv.FirstChild;

         if Item.Priv.Next/=null then
            Item.Priv.Next.Priv.Last:=Object_ClassAccess(Item);
         else
            Par.Priv.LastChild:=Object_ClassAccess(Item);
         end if;

         Par.Priv.FirstChild:=Object_ClassAccess(Item);

      end if;

   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize
     (Item : access Object_Type) is

      ItemVal : Object_Access;

   begin
      ItemVal:=Object_Access(Item);

      if Item.Priv.Parent/=null then
         if Item.Priv.Next/=null then
            Item.Priv.Next.Priv.Last:=Item.Priv.Last;
         else
            Item.Priv.Parent.Priv.LastChild:=Item.Priv.Last;
         end if;

         if Item.Priv.Last/=null then
            Item.Priv.Last.Priv.Next:=Item.Priv.Next;
         else
            Item.Priv.Parent.Priv.FirstChild:=Item.Priv.Next;
         end if;
      end if;

      Free(ItemVal);
   end Finalize;
   ---------------------------------------------------------------------------

   procedure Initialize
     (Context : Context_ClassAccess) is
   begin
      Put("Init Context Basics");
      New_Line;
      -- TODO: Use specialized objects
      Context.WindowArea      := new Object_Type;
      Context.ModalArea       := new Object_Type;
      Context.ContextMenuArea := new Object_Type;
      Context.WindowArea.Context      := Context;
      Context.ModalArea.Context       := Context;
      Context.ContextMenuArea.Context := Context;
      Resize
        (Context => Context,
         Height  => Context.Bounds.Height,
         Width   => Context.Bounds.Width);
   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize
     (Context : in out Context_Type) is
   begin
      Put("Finit");
      New_Line;
      Context.WindowArea.Finalize;
      Context.ModalArea.Finalize;
      Context.ContextMenuArea.Finalize;
   end;
   ---------------------------------------------------------------------------

end GUI;
