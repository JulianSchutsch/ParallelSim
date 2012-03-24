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

pragma Ada_2005;

with BoundsCalc; use BoundsCalc;
with Config.Implementations;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Basics; use Basics;

package GUI is

   FailedToCreateContext : Exception;
   InvalidContext : Exception;
   FailedToDestroyContext : Exception;

   type Object_Type;
   type Object_ClassAccess is access all Object_Type'Class;

   type Context_Type;
   type Context_ClassAccess is access all Context_Type'Class;
   ---------------------------------------------------------------------------

   type Object_Private is private;
   type Object_Type is tagged
      record
         CallBackObject : AnyObject_ClassAccess;
         Context        : Context_ClassAccess;
         Priv           : Object_Private;
      end record;

   procedure SetBounds
     (Object : in out Object_Type'Class;
      Bounds : Bounds_Type);

   function GetBounds
     (Object : Object_Type'Class)
      return Bounds_Type;
   ---------------------------------------------------------------------------

   type OnCloseContext_Access is
     access procedure (CallBackObject : AnyObject_ClassAccess);

   type Context_Type is tagged
      record
         CallBackObject : AnyObject_ClassAccess := null;
         OnClose        : OnCloseContext_Access := null;
         RootObject     : Object_ClassAccess    := null;
         -- Read only
         Bounds         : Bounds_Type;
      end record;

   type Canvas_Type is tagged private;
   type Canvas_ClassAccess is access all Canvas_Type'Class;

   procedure NewCanvas
     (Context : in out Context_Type;
      Object  : Object_ClassAccess;
      Height  : Natural;
      Width   : Natural;
      Canvas  : out Canvas_ClassAccess) is null;

   procedure FreeCanvas
     (Canvas : in out Canvas_ClassAccess) is null;
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

   procedure Resize
     (Context : in out Context_Type;
      Height  : Integer;
      Width   : Integer);

   type Object_Private is
      record
         Canvasse  : Canvas_ClassAccess;
         Bounds    : Bounds_Type;
         AbsBounds : AbsBounds_Type;
      end record;

   type Canvas_Type is tagged
      record
         null;
      end record;

end GUI;
