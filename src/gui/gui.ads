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
--   11.Mar 2012 Julian Schutsch
--     - Original version

-- Reason for implementation
--   Provide a basis for a simple GUI framework solving essential tasks as
--   nested rectangle(object) management, mouse and keyboard signal delievery
--   and canvasse.
--   Actual implementation is part of child packages.

-- Usage
--   Objects in this package should be treated as abstract.
--   Before usage, a context implementation must be selected using the
--   Implementations sub-package.
--   With the implementation a new (render) context can be created.
--
--   Each context has three root objects:
--     * WindowArea   : For ordinary objects, lowest layer
--     * ModalArea    : Similar to WindowArea, but blocks all signals if
--                      activated
--     * ContextArea  : Highest layer for special context specific tasks
--                      like for example context menus or combo box
--                      lists.
--   Objects are a combination of a basic functionality and a theme.
--   To actually create an object a theme has to be selected in GUI.Themes.
--   As a parent any of the three root objects or any other object can be
--   chosen.

pragma Ada_2005;

with BoundsCalc; use BoundsCalc;
with Config.Implementations;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Basics; use Basics;
with Canvas;

package GUI is

   FocusRedirectionToNull : Exception;

   type FocusStyle_Enum is
     (FocusStyleNone,
      FocusStyleAccept,
      FocusStyleContainer,
      FocusStyleRedirect);

   type Key_Enum is
     (KeyUnknown,
      KeyUp,
      KeyLeft,
      KeyRight,
      KeyDown,
      KeyBackspace,
      KeyReturn,
      KeyEnter,
      KeyHome,
      KeyEnd,
      KeyDelete);

   type MouseButton_Enum is
     (LeftButton,
      RightButton);
   type MouseButton_Array is array(MouseButton_Enum) of Boolean;

   NoMouseButtons : MouseButton_Array:=(False,False);

   FailedToCreateContext  : Exception;
   InvalidContext         : Exception;
   FailedToDestroyContext : Exception;

   type Render_Enum is
     (RenderCanvasse,
      RenderCustom);

   type Context_Type;
   type Context_ClassAccess is access all Context_Type'Class;
   ---------------------------------------------------------------------------

   type Object_Public is new AnyObject_Type with
      record
         Render         : Render_Enum:=RenderCanvasse;
         CallBackObject : AnyObject_ClassAccess;
         Context        : Context_ClassAccess;
         FocusStyle     : FocusStyle_Enum:=FocusStyleNone;
         Focussed       : Boolean:=False;
         TopHeightConstraint : Constraint_Type;
         LeftWidthConstraint : Constraint_Type;
         -- Read only
      end record;

   type Object_Type is new Object_Public with private;
   type Object_Access is access all Object_Type;
   type Object_ClassAccess is access all Object_Type'Class;

   procedure Focus
     (Item : access Object_Type) is null;

   procedure Defocus
     (Item : access Object_Type) is null;

   procedure Finalize
     (Item : access Object_Type);

   function CharacterInput
     (Item  : access Object_Type;
      Chars : Unbounded_String)
      return Boolean;

   function KeyDown
     (Item : access Object_Type;
      Key  : Key_Enum)
      return Boolean;

   function KeyUp
     (Item : access Object_Type;
      Key  : Key_Enum)
      return Boolean;

   function MouseDown
     (Item   : access Object_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer)
      return Boolean;

   procedure MouseUp
     (Item   : access Object_Type;
      Button : MouseButton_Enum;
      X      : Integer;
      Y      : Integer) is null;

   procedure MouseMove
     (Item   : access Object_Type;
      X      : Integer;
      Y      : Integer) is null;

   procedure SetBounds
     (Object  : access Object_Type;
      Top     : Integer;
      Left    : Integer;
      Height  : Integer;
      Width   : Integer;
      Visible : Boolean);

   function GetBounds
     (Object : access Object_Type)
      return Bounds_Type;

   function GetPrevBounds
     (Object : access Object_Type)
      return Bounds_Type;

   procedure SetAnchors
     (Object : access Object_Type;
      Top    : Boolean;
      Left   : Boolean;
      Right  : Boolean;
      Bottom : Boolean);

   function GetClient
     (Object : access Object_Type)
      return Object_ClassAccess;

   procedure SetClient
     (Object : access Object_Type;
      Client : Object_ClassAccess);

   procedure Resize
     (Item : access Object_Type) is null;

   procedure SetFocus
     (Item : access Object_Type);

   procedure BringToFront
     (Item : Object_ClassAccess);

   procedure SetFocusObject
     (Item   : access Object_Type;
      Object : Object_ClassAccess);
   ---------------------------------------------------------------------------

   type OnCloseContext_Access is
     access procedure (CallBackObject : AnyObject_ClassAccess);

   type Context_Private is private;

   type Context_Type is tagged
      record
         CallBackObject      : AnyObject_ClassAccess := null;
         OnClose             : OnCloseContext_Access := null;
         WindowArea          : Object_ClassAccess    := null;
         ModalArea           : Object_ClassAccess    := null;
         ContextArea         : Object_ClassAccess    := null;
         -- Read only
         Bounds         : Bounds_Type;
         Priv           : Context_Private;
      end record;

   type Canvas_Type is new Canvas.Canvas_Type with private;
   type Canvas_ClassAccess is access all Canvas_Type'Class;

   procedure SetBounds
     (Canvas  : access Canvas_Type;
      Top     : Integer;
      Left    : Integer;
      Height  : Integer;
      Width   : Integer;
      Visible : Boolean);

   procedure SetAnchors
     (Canvas : access Canvas_Type;
      Top    : Boolean;
      Left   : Boolean;
      Right  : Boolean;
      Bottom : Boolean);

   procedure NewCanvas
     (Context : in out Context_Type;
      Object  : Object_ClassAccess;
      Height  : Positive;
      Width   : Positive;
      Canvas  : out Canvas_ClassAccess) is null;

   procedure FreeCanvas
     (Context : in out Context_Type;
      Canvas  : in out Canvas_ClassAccess) is null;
   ---------------------------------------------------------------------------
   type Context_Constructor is
     access function
       (Configuration : Config.Config_Type;
        Node          : Unbounded_String)
        return Context_ClassAccess;

   type Context_Destructor is
     access procedure
       (Context : Context_ClassAccess);
   ---------------------------------------------------------------------------

   type Implementation_Type is
      record
         NewContext  : Context_Constructor:=null;
         FreeContext : Context_Destructor:=null;
      end record;

   package Implementations is new Config.Implementations
     (Implementation_Type => Implementation_Type,
      IdentifierKey       => To_Unbounded_String("GUIImplementation"));

private

   type Context_Private is
      record
         MouseButtonsPressed : MouseButton_Array  := NoMouseButtons;
         MouseSelection      : Object_ClassAccess := null;
         FocusObject         : Object_ClassAccess := null;
      end record;

   procedure Initialize
     (Item   : Object_Access;
      Parent : Object_ClassAccess);

   procedure Resize
     (Context : Context_ClassAccess;
      Height  : Integer;
      Width   : Integer);

   procedure Initialize
     (Context : Context_ClassAccess);

   procedure Finalize
     (Context : in out Context_Type);
   ---------------------------------------------------------------------------

   type Object_Type is new Object_Public with
      record
         Canvasse            : Canvas_ClassAccess:=null;
         Bounds              : Bounds_Type;
         PrevBounds          : Bounds_Type;
         AbsBounds           : AbsBounds_Type;
         Anchors             : Anchors_Type;
         Next                : Object_ClassAccess:=null;
         Last                : Object_ClassAccess:=null;
         Parent              : Object_ClassAccess:=null;
         FirstChild          : Object_ClassAccess:=null;
         LastChild           : Object_ClassAccess:=null;
         Client              : Object_ClassAccess:=null;
         FocusObject         : Object_ClassAccess:=null;
      end record;

   ---------------------------------------------------------------------------

   type Canvas_Type is new Canvas.Canvas_Type with
      record
         Next    : Canvas_ClassAccess;
         Last    : Canvas_ClassAccess;
         Bounds  : Bounds_Type;
         Anchors : Anchors_Type;
         Object  : Object_ClassAccess;
      end record;

   procedure AddCanvas
     (Object : Object_ClassAccess;
      Canvas : Canvas_ClassAccess);

   procedure RemoveCanvas
     (Canvas : Canvas_ClassAccess);

   procedure MouseDown
     (Context     : Context_ClassAccess;
      MouseButton : MouseButton_Enum;
      AbsX        : Integer;
      AbsY        : Integer);

   procedure MouseUp
     (Context     : Context_ClassAccess;
      MouseButton : MouseButton_Enum;
      AbsX        : Integer;
      AbsY        : Integer);

   procedure MouseMove
     (Context : Context_ClassAccess;
      AbsX    : Integer;
      AbsY    : Integer);

   procedure CharacterInput
     (Context : Context_ClassAccess;
      Chars   : Unbounded_String);

   procedure KeyDown
     (Context : Context_ClassAccess;
      Key     : Key_Enum);

   procedure KeyUp
     (Context : Context_ClassAccess;
      Key     : Key_Enum);

end GUI;
