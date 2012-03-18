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

with Bounds;
with Config.Implementations;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Basics; use Basics;

package GUI is

   FailedToCreateContext : Exception;

   type Object_Type is tagged private;
   type Object_ClassAccess is access all Object_Type'Class;

   procedure SetBounds
     (Object    : in out Object_Type'Class;
      NewBounds : Bounds.Bounds_Type);

   function GetBounds
     (Object : Object_Type'Class)
      return Bounds.Bounds_Type;

   type Canvas_Type is tagged private;
   type Canvas_ClassAccess is access all Canvas_Type'Class;

   type Context_Type is new Object_Type with private;
   type Context_ClassAccess is access all Context_Type'Class;

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
       (Configuration : StringStringMap.Map)
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

   type Object_Type is tagged
      record
         Canvasse  : Canvas_ClassAccess;
         RelBounds : Bounds.Bounds_Type;
         AbsBounds : Bounds.AbsBounds_Type;
      end record;

   type Context_Type is new Object_Type with
      record
         null;
      end record;

   type Canvas_Type is tagged
      record
         null;
      end record;

end GUI;
