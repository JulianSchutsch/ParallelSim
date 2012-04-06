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

package body GUI is

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Object_Type,
      Name   => Object_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Canvas_Type'Class,
      Name   => Canvas_ClassAccess);
   ---------------------------------------------------------------------------

   procedure CharacterInput
     (Context : Context_ClassAccess;
      Chars   : Unbounded_String) is

   begin

      if Context.Priv.FocusObject/=null then
         if not Context.Priv.FocusObject.CharacterInput(Chars) then
            Put("Character rejected:");
            Put(To_String(Chars));
            New_Line;
         end if;
      end if;

   end CharacterInput;
   ---------------------------------------------------------------------------

   procedure KeyDown
     (Context : Context_ClassAccess;
      Key     : Key_Type) is
   begin

      if Context.Priv.FocusObject/=null then
         if not Context.Priv.FocusObject.KeyDown(Key) then
            Put("Key rejected");
            New_Line;
         end if;
      end if;

   end KeyDown;
   ---------------------------------------------------------------------------

   procedure KeyUp
     (Context : Context_ClassAccess;
      Key     : Key_Type) is
   begin

      if Context.Priv.FocusObject/=null then
         if not Context.Priv.FocusObject.KeyUp(Key) then
            Put("Key up rejected");
            New_Line;
         end if;
      end if;

   end KeyUp;
   ---------------------------------------------------------------------------

   procedure ClearFocusTree
     (Object : Object_ClassAccess) is

      Cursor : Object_ClassAccess;

   begin

      Cursor:=Object.Context.Priv.FocusObject;

      while Cursor/=null loop

         if (Cursor.FocusStyle=FocusStyleContainer) and
           (Cursor.FocusObject=Object.Context.Priv.FocusObject) then

            Cursor.FocusObject:=null;

         end if;

         Cursor.Focussed:=False;
         Cursor.Defocus;

         Cursor:=Cursor.Priv.Parent;

      end loop;

   end ClearFocusTree;
   ---------------------------------------------------------------------------

   procedure SetFocusTree
     (Object : Object_ClassAccess) is

         Cursor : Object_ClassAccess;

   begin

      ClearFocusTree(Object);

      Object.Context.Priv.FocusObject:=Object;
      Cursor:=Object;

      while Cursor/=null loop

         if (Cursor.FocusStyle=FocusStyleContainer) and
           (Cursor.FocusObject/=Object) then

            Cursor.FocusObject:=null;

         end if;

         Cursor.Focussed:=True;
         Cursor.Focus;

         Cursor:=Cursor.Priv.Parent;

      end loop;

   end;
   ---------------------------------------------------------------------------

   procedure SetFocus
     (Item : access Object_Type) is

      Cursor : Object_ClassAccess;

   begin

      Cursor:=Object_ClassAccess(Item);

      while (Cursor/=null) loop
         Put(Cursor.all'Address);
         New_Line;

         case Cursor.FocusStyle is

            when FocusStyleNone =>
               Cursor:=Cursor.Priv.Parent;

            when FocusStyleAccept =>
               Put("Accept..therefore set");
               New_Line;
               SetFocusTree(Cursor);
               return;

            when FocusStyleContainer =>

               Put("Container Reflection");
               New_Line;
               if Cursor.FocusObject/=null then
                  Cursor:=Cursor.FocusObject;
               else
                  SetFocusTree(Cursor);
                  return;
               end if;

            when FocusStyleRedirect =>
               Put("Redirect");
               New_Line;
               if Cursor.FocusObject=null then
                  raise FocusRedirectionToNull;
               end if;
               SetFocusTree(Cursor);
               return;

         end case;

      end loop;

   end;
   ---------------------------------------------------------------------------

   function KeyDown
     (Item : access Object_Type;
      Key  : Key_Type)
      return Boolean is

      pragma Unreferenced(Item);
      pragma Unreferenced(Key);

   begin
      return True;
   end KeyDown;
   ---------------------------------------------------------------------------

   function KeyUp
     (Item : access Object_Type;
      Key  : Key_Type)
      return Boolean is

      pragma Unreferenced(Item);
      pragma Unreferenced(Key);

   begin
      return True;
   end KeyUp;
   ---------------------------------------------------------------------------

   function CharacterInput
     (Item  : access Object_Type;
      Chars : Unbounded_String)
      return Boolean is

      pragma Unreferenced(Item);
      pragma Unreferenced(Chars);

   begin
      return False;
   end CharacterInput;

   function MouseDown
     (Item   : access Object_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer)
      return Boolean is

      pragma Unreferenced(Item);
      pragma Unreferenced(Button);
      pragma Unreferenced(X);
      pragma Unreferenced(Y);

   begin
      return False;
   end MouseDown;
   ---------------------------------------------------------------------------

   -- Global Mousedown procedure which either delivers the mouse signal to a
   -- previously selected object (mouse select) or finds a candidate to be
   -- selected.
   procedure MouseDown
     (Context     : Context_ClassAccess;
      MouseButton : MouseButton_Enum;
      AbsX        : Integer;
      AbsY        : Integer) is

      function SubMouseDown
        (Object : Object_ClassAccess)
         return Boolean is

         ObjectCursor    : Object_ClassAccess;
         MouseEventTaken : Boolean;

      begin

         if TestInsideAbsBounds
           (AbsBounds => Object.Priv.AbsBounds,
            AbsX      => AbsX,
            AbsY      => AbsY) then

            -- Test if any nested object wants the mouse signal
            ObjectCursor:=Object.Priv.FirstChild;

            while ObjectCursor/=null loop
               if SubMouseDown(ObjectCursor) then
                  return True;
               end if;
               ObjectCursor:=ObjectCursor.Priv.Next;
            end loop;

            -- Test if this object wants the mouse signal
            declare
               Bounds : AbsBounds_Type renames
                 Object.Priv.AbsBounds;
            begin
               MouseEventTaken:=Object.MouseDown
                 (Button => MouseButton,
                  X      => AbsX-Bounds.AbsLeft+Bounds.AbsSubLeft,
                  Y      => AbsY-Bounds.AbsTop+Bounds.AbsSubTop);
            end;

            if MouseEventTaken then

               Context.Priv.MouseSelection := Object;
               return True;

            end if;

         end if;

         return False;

      end SubMouseDown;
      ------------------------------------------------------------------------

   begin

      if Context.Priv.MouseButtonsPressed=NoMouseButtons then

         Context.Priv.MouseSelection:=null;

         declare
            Result : Boolean;
            pragma Unreferenced(Result);
         begin
            if not SubMouseDown(Context.ContextArea) then
               if not SubMouseDown(Context.ModalArea) then
                  Result:=SubMouseDown(Context.WindowArea);
               end if;
            end if;
         end;

         if Context.Priv.MouseSelection/=null then
            Put("Set Focus");
            Put(Context.Priv.MouseSelection.all'Address);
            New_Line;
            Context.Priv.MouseSelection.SetFocus;
         end if;

      else

         if Context.Priv.MouseSelection/=null then

            declare
               MouseEventTaken : Boolean;
               pragma Unreferenced(MouseEventTaken);
            begin

               declare
                  Bounds : AbsBounds_Type renames
                    Context.Priv.MouseSelection.Priv.AbsBounds;
               begin
                  MouseEventTaken:=Context.Priv.MouseSelection.MouseDown
                    (Button => MouseButton,
                     X      => AbsX-Bounds.AbsLeft+Bounds.AbsSubLeft,
                     Y      => AbsY-Bounds.AbsTop+Bounds.AbsSubTop);
               end;

            end;

         end if;

      end if;

      Context.Priv.MouseButtonsPressed(MouseButton):=True;

   end MouseDown;
   ---------------------------------------------------------------------------

   procedure MouseUp
     (Context     : Context_ClassAccess;
      MouseButton : MouseButton_Enum;
      AbsX        : Integer;
      AbsY        : Integer) is

   begin

      if Context.Priv.MouseSelection/=null then

         declare
            Bounds : AbsBounds_Type renames
              Context.Priv.MouseSelection.Priv.AbsBounds;
         begin
            Context.Priv.MouseSelection.MouseUp
              (Button => MouseButton,
               X      => AbsX-Bounds.AbsLeft+Bounds.AbsSubLeft,
               Y      => AbsY-Bounds.AbsTop+Bounds.AbsSubTop);
         end;
      end if;

      Context.Priv.MouseButtonsPressed(MouseButton):=False;
      if Context.Priv.MouseButtonsPressed=NoMouseButtons then
         Context.Priv.MouseSelection:=null;
      end if;

   end MouseUp;
   ---------------------------------------------------------------------------

   procedure MouseMove
     (Context : Context_ClassAccess;
      AbsX    : Integer;
      AbsY    : Integer) is
   begin

      if Context.Priv.MouseSelection/=null then

         declare
            Bounds : AbsBounds_Type renames
              Context.Priv.MouseSelection.Priv.AbsBounds;
         begin
            Context.Priv.MouseSelection.MouseMove
              (X => AbsX-Bounds.AbsLeft+Bounds.AbsSubLeft,
               Y => AbsY-Bounds.AbsTop+Bounds.AbsSubTop);
         end;

      end if;

   end MouseMove;
   ---------------------------------------------------------------------------

   procedure ResizeTree
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
         ResizeTree(ObjectCursor);
         ObjectCursor:=ObjectCursor.Priv.Next;
      end loop;
      ------------------------------------------------------------------------

      if (Object.Priv.Bounds.Height/=Object.Priv.PrevBounds.Height)
        or (Object.Priv.Bounds.Width/=Object.Priv.PrevBounds.Width) then

         Object.Resize;

      end if;

      Object.Priv.PrevBounds := Object.Priv.Bounds;

   end ResizeTree;
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

      if Context.WindowArea/=null then
         Context.WindowArea.SetBounds
           (Top     => Context.Bounds.Top,
            Left    => Context.Bounds.Left,
            Height  => Context.Bounds.Height,
            Width   => Context.Bounds.Width,
            Visible => Context.Bounds.Visible);
      end if;
      if Context.ModalArea/=null then
         Context.ModalArea.SetBounds
           (Top     => Context.Bounds.Top,
            Left    => Context.Bounds.Left,
            Height  => Context.Bounds.Height,
            Width   => Context.Bounds.Width,
            Visible => Context.Bounds.Visible);
      end if;
      if Context.ContextArea/=null then
         Context.ContextArea.SetBounds
           (Top     => Context.Bounds.Top,
            Left    => Context.Bounds.Left,
            Height  => Context.Bounds.Height,
            Width   => Context.Bounds.Width,
            Visible => Context.Bounds.Visible);
      end if;
   end Resize;
   ---------------------------------------------------------------------------

   procedure SetAnchors
     (Object : access Object_Type;
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
     (Canvas  : access Canvas_Type;
      Top     : Integer;
      Left    : Integer;
      Height  : Integer;
      Width   : Integer;
      Visible : Boolean) is

   begin

      Canvas.Bounds:=
        (Top     => Top,
         Left    => Left,
         Height  => Height,
         Width   => Width,
         Visible => Visible);

      StoreAnchors
        (Anchors => Canvas.Anchors,
         ClientBounds => Canvas.Bounds,
         ParentBounds => Canvas.Object.Priv.Bounds);

   end SetBounds;
   ---------------------------------------------------------------------------

   procedure SetAnchors
     (Canvas : access Canvas_Type;
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
     (Object  : access Object_Type;
      Top     : Integer;
      Left    : Integer;
      Height  : Integer;
      Width   : Integer;
      Visible : Boolean) is
   begin

      Object.Priv.Bounds:=
        (Top     => Top,
         Left    => Left,
         Height  => Height,
         Width   => Width,
         Visible => Visible);

      ResizeTree(Object_ClassAccess(Object));

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
      if Par=null then
         return;
      end if;

      while Par.Client/=null loop
         Par:=Par.Client;
      end loop;

      Item.Priv.Parent:=Par;

      Item.Context := Par.Context;

      Item.Priv.Next:=Par.Priv.FirstChild;

      if Item.Priv.Next/=null then
         Item.Priv.Next.Priv.Last:=Object_ClassAccess(Item);
      else
         Par.Priv.LastChild:=Object_ClassAccess(Item);
      end if;

      Par.Priv.FirstChild:=Object_ClassAccess(Item);

   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize
     (Item : access Object_Type) is

      ItemVal : Object_Access;

   begin

      -- TODO: This is not enough, there can be referenced through containers
      -- be left which MUST be removed too
      if Item.Context.Priv.FocusObject=Object_ClassAccess(Item) then
         ClearFocusTree(Object_ClassAccess(Item));
      end if;

      -- Finalize all Childs

      declare
         Object     : Object_ClassAccess;
         NextObject : Object_ClassAccess;
      begin
         Object:=Item.Priv.FirstChild;

         while Object/=null loop
            NextObject:=Object.Priv.Next;
            Object.Finalize;
            Object:=NextObject;
         end loop;
      end;

      -- Free all Canvasse
      declare
         Canvas     : Canvas_ClassAccess;
         NextCanvas : Canvas_ClassAccess;
      begin
         Canvas := Item.Priv.Canvasse;

         while Canvas/=null loop
            NextCanvas:=Canvas.Next;
            Item.Context.FreeCanvas(Canvas);
            Canvas:=NextCanvas;
         end loop;
      end;

      -- Remove Object from the tree
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
      -- TODO: Use specialized objects
      Context.WindowArea          := new Object_Type;
      Context.ModalArea           := new Object_Type;
      Context.ContextArea         := new Object_Type;
      Context.WindowArea.Context  := Context;
      Context.ModalArea.Context   := Context;
      Context.ContextArea.Context := Context;
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
      Context.ContextArea.Finalize;
   end;
   ---------------------------------------------------------------------------

end GUI;
