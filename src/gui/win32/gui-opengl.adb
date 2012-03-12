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

package body GUI.OpenGL is

   type Context_Type is new GUI.Context_Type with
      record
         null;
      end record;
   type Context_Access is access all Context_Type;

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Context_Type,
      Name   => Context_Access);

   function NewContext
     (Configuration : StringStringMap.Map)
      return Context_ClassAccess is
      pragma Warnings(Off,Configuration);

      Context : Context_Access;
   begin
      Context:=new Context_Type;
      return Context_ClassAccess(Context);
   end NewContext;

   procedure FreeContext
     (Context : Context_ClassAccess) is
      Cont : Context_Access;
   begin
      Cont := Context_Access(Context);
      Free(Cont);
   end FreeContext;

   Implementation : constant Implementation_Type:=
     (NewContext  => NewContext'Access,
      FreeContext => FreeContext'Access);

   procedure Register is
   begin
      Implementations.Register
        (Identifier     => To_Unbounded_String("OpenGL"),
         Implementation => Implementation);
   end Register;
   ---------------------------------------------------------------------------

end GUI.OpenGL;
