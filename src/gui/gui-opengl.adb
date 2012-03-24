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

with GUI.OpenGL.Native;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with OpenGL; use OpenGL;

package body GUI.OpenGL is

   procedure Paint
     (Context : in out Context_Type) is

   begin
      Put("Paint Called");
      Put(Context.Bounds.Width);
      Put(Context.Bounds.Height);
      New_Line;
      glViewPort
        (x => 0,
         y => 0,
         width  => GLsizei_Type(Context.Bounds.Width),
         height => GLsizei_Type(Context.Bounds.Height));

      glMatrixMode(GL_PROJECTION);
      glLoadIdentity;
      glOrtho
        (left    => 0.0,
         right   => GLdouble(Context.Bounds.Width),
         bottom  => GLdouble(Context.Bounds.Height),
         top     => 0.0,
         nearVal => -1.0,
         farVal  => 1.0);

      glMatrixMode(GL_MODELVIEW);
      glLoadIdentity;

      glClearColor
        (red   => 0.0,
         green => 0.0,
         blue  => 1.0,
         alpha => 1.0);

      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
      Put(Integer(glGetError));
      New_Line;
   end Paint;

   procedure Register is
   begin
      GUI.OpenGL.Native.Register;
   end Register;

end GUI.OpenGL;
