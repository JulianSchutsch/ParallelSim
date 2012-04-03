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
--   18.Mar 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

package GUI.OpenGL is

   procedure Register;

   procedure UnRegister;

private
   type Context_Type is new GUI.Context_Type with
      record
         null;
      end record;

   overriding
   procedure NewCanvas
     (Context : in out Context_Type;
      Object  : Object_ClassAccess;
      Height  : Positive;
      Width   : Positive;
      Canvas  : out Canvas_ClassAccess);

   overriding
   procedure FreeCanvas
     (Context : in out Context_Type;
      Canvas  : Canvas_ClassAccess);

   procedure Paint
     (Context : in out Context_Type);

end GUI.OpenGL;
