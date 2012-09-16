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

with Xlib; use Xlib;
with OpenGL; use OpenGL;
with ProcessLoop;
with Interfaces.C.Strings;
with glX;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Unchecked_Conversion;
with Basics; use Basics;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Config;
with GUIMouse; use GUIMouse;
with GUIKeys; use GUIKeys;

package body OpenGL.Context.Xlib is


   Implementation : constant Implementation_Type:=
     (NewContext  => NewContext'Access);
   Identifier     : constant Unbounded_String:=To_Unbounded_String("OpenGL");

   procedure Register is
   begin

      Implementations.Register
        (Identifier     => Identifier,
         Implementation => Implementation);

   end Register;
   ---------------------------------------------------------------------------

   procedure UnRegister is
   begin

      Implementations.UnRegister
        (Identifier => Identifier);

   end UnRegister;

end OpenGL.Context.Xlib;
